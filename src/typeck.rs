use std::collections::{HashMap, HashSet};
use std::error;
use std::fmt;

use crate::ast;
use crate::core::*;

#[derive(Debug)]
pub struct SyntaxError(String);
impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&self.0)
    }
}
impl error::Error for SyntaxError {}
type Result<T> = std::result::Result<T, Box<dyn error::Error>>;

struct Bindings {
    m: HashMap<String, Value>,
    changes: Vec<(String, Option<Value>)>,
}
impl Bindings {
    fn new() -> Self {
        Self {
            m: HashMap::new(),
            changes: Vec::new(),
        }
    }

    fn get(&self, k: &str) -> Option<Value> {
        self.m.get(k).copied()
    }

    fn insert(&mut self, k: String, v: Value) {
        let old = self.m.insert(k.clone(), v);
        self.changes.push((k, old));
    }

    fn unwind(&mut self, n: usize) {
        while self.changes.len() > n {
            let (k, old) = self.changes.pop().unwrap();
            match old {
                Some(v) => self.m.insert(k, v),
                None => self.m.remove(&k),
            };
        }
    }

    fn in_child_scope<T>(&mut self, cb: impl FnOnce(&mut Self) -> T) -> T {
        let n = self.changes.len();
        let res = cb(self);
        self.unwind(n);
        res
    }
}

fn check_expr(engine: &mut TypeCheckerCore, bindings: &mut Bindings, expr: &ast::Expr) -> Result<Value> {
    use ast::Expr::*;

    match expr {
        Call(func_expr, arg_expr) => {
            let func_type = check_expr(engine, bindings, func_expr)?;
            let arg_type = check_expr(engine, bindings, arg_expr)?;

            let (ret_type, ret_bound) = engine.var();
            let bound = engine.func_use(arg_type, ret_bound);
            engine.flow(func_type, bound)?;
            Ok(ret_type)
        }
        Case(tag, val_expr) => {
            let val_type = check_expr(engine, bindings, val_expr)?;
            Ok(engine.case(vec![(tag.clone(), val_type)]))
        }
        FieldAccess(lhs_expr, name) => {
            let lhs_type = check_expr(engine, bindings, lhs_expr)?;

            let (field_type, field_bound) = engine.var();
            let bound = engine.obj_use(vec![(name.clone(), field_bound)]);
            engine.flow(lhs_type, bound)?;
            Ok(field_type)
        }
        FuncDef(arg_name, body_expr) => {
            let (arg_type, arg_bound) = engine.var();
            bindings.in_child_scope(|bindings| {
                bindings.insert(arg_name.clone(), arg_type);
                let body_type = check_expr(engine, bindings, body_expr)?;
                Ok(engine.func(arg_bound, body_type))
            })
        }
        If(cond_expr, then_expr, else_expr) => {
            let cond_type = check_expr(engine, bindings, cond_expr)?;
            let bound = engine.bool_use();
            engine.flow(cond_type, bound)?;

            let then_type = check_expr(engine, bindings, then_expr)?;
            let else_type = check_expr(engine, bindings, else_expr)?;

            let (merged, merged_bound) = engine.var();
            engine.flow(then_type, merged_bound)?;
            engine.flow(else_type, merged_bound)?;
            Ok(merged)
        }
        Let((name, var_expr), rest_expr) => {
            let var_type = check_expr(engine, bindings, var_expr)?;
            bindings.in_child_scope(|bindings| {
                bindings.insert(name.clone(), var_type);
                check_expr(engine, bindings, rest_expr)
            })
        }
        LetRec(defs, rest_expr) => bindings.in_child_scope(|bindings| {
            let mut temp_bounds = Vec::with_capacity(defs.len());
            for (name, _) in defs {
                let (temp, temp_bound) = engine.var();
                bindings.insert(name.clone(), temp);
                temp_bounds.push(temp_bound);
            }

            for ((_, expr), bound) in defs.iter().zip(temp_bounds) {
                let var_type = check_expr(engine, bindings, expr)?;
                engine.flow(var_type, bound)?;
            }

            check_expr(engine, bindings, rest_expr)
        }),
        Literal(val) => {
            use ast::Literal::*;
            Ok(match val {
                Bool(_) => engine.bool(),
            })
        }
        Match(match_expr, cases) => {
            let match_type = check_expr(engine, bindings, match_expr)?;
            let (result_type, result_bound) = engine.var();

            let mut case_names = HashSet::with_capacity(cases.len());
            let mut case_type_pairs = Vec::with_capacity(cases.len());
            for ((tag, name), rhs_expr) in cases {
                if !case_names.insert(&*name) {
                    return Err(SyntaxError(format!("Repeated match case {}", name)).into());
                }

                let (wrapped_type, wrapped_bound) = engine.var();
                case_type_pairs.push((tag.clone(), wrapped_bound));

                bindings.in_child_scope(|bindings| {
                    bindings.insert(name.clone(), wrapped_type);
                    let rhs_type = check_expr(engine, bindings, rhs_expr)?;
                    engine
                        .flow(rhs_type, result_bound)
                        .or_else(|e| Err(Box::new(e) as Box<dyn error::Error>))
                })?;
            }

            let bound = engine.case_use(case_type_pairs);
            engine.flow(match_type, bound)?;

            Ok(result_type)
        }
        Record(fields) => {
            let mut field_names = HashSet::with_capacity(fields.len());
            let mut field_type_pairs = Vec::with_capacity(fields.len());
            for (name, expr) in fields {
                if !field_names.insert(&*name) {
                    return Err(SyntaxError(format!("Repeated field name {}", name)).into());
                }

                let t = check_expr(engine, bindings, expr)?;
                field_type_pairs.push((name.clone(), t));
            }
            Ok(engine.obj(field_type_pairs))
        }
        Variable(name) => bindings
            .get(name.as_str())
            .ok_or_else(|| SyntaxError(format!("Undefined variable {}", name)).into()),
    }
}

fn check_toplevel(engine: &mut TypeCheckerCore, bindings: &mut Bindings, def: &ast::TopLevel) -> Result<()> {
    use ast::TopLevel::*;
    match def {
        Expr(expr) => {
            check_expr(engine, bindings, expr)?;
        }
        LetDef((name, var_expr)) => {
            let var_type = check_expr(engine, bindings, var_expr)?;
            bindings.insert(name.clone(), var_type);
        }
        LetRecDef(defs) => {
            let mut temp_bounds = Vec::with_capacity(defs.len());
            for (name, _) in defs {
                let (temp, temp_bound) = engine.var();
                bindings.insert(name.clone(), temp);
                temp_bounds.push(temp_bound);
            }

            for ((_, expr), bound) in defs.iter().zip(temp_bounds) {
                let var_type = check_expr(engine, bindings, expr)?;
                engine.flow(var_type, bound)?;
            }
        }
    };
    Ok(())
}

pub struct TypeckState {
    core: TypeCheckerCore,
    bindings: Bindings,
}
impl TypeckState {
    pub fn new() -> Self {
        Self {
            core: TypeCheckerCore::new(),
            bindings: Bindings::new(),
        }
    }

    pub fn check_script(&mut self, parsed: &[ast::TopLevel]) -> Result<()> {
        // Create temporary copy of the entire type state so we can roll
        // back all the changes if the script contains an error.
        let mut temp = self.core.clone();

        for item in parsed {
            if let Err(e) = check_toplevel(&mut self.core, &mut self.bindings, item) {
                // Roll back changes to the type state and bindings
                std::mem::swap(&mut self.core, &mut temp);
                self.bindings.unwind(0);
                return Err(e);
            }
        }

        // Now that script type-checked successfully, make the global definitions permanent
        // by removing them from the changes rollback list
        self.bindings.changes.clear();
        Ok(())
    }
}
