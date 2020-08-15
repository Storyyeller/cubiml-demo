use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::error;
use std::fmt;
use std::rc::Rc;

use crate::ast;
use crate::core::*;
use crate::spans::{Span, SpannedError as SyntaxError};

type Result<T> = std::result::Result<T, SyntaxError>;

#[derive(Clone)]
enum Scheme {
    Mono(Value),
    Poly(Rc<dyn Fn(&mut TypeCheckerCore) -> Result<Value>>),
}

struct Bindings {
    m: HashMap<String, Scheme>,
    changes: Vec<(String, Option<Scheme>)>,
}
impl Bindings {
    fn new() -> Self {
        Self {
            m: HashMap::new(),
            changes: Vec::new(),
        }
    }

    fn get(&self, k: &str) -> Option<&Scheme> {
        self.m.get(k)
    }

    fn insert_scheme(&mut self, k: String, v: Scheme) {
        let old = self.m.insert(k.clone(), v);
        self.changes.push((k, old));
    }

    fn insert(&mut self, k: String, v: Value) {
        self.insert_scheme(k, Scheme::Mono(v))
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
        BinOp((lhs_expr, lhs_span), (rhs_expr, rhs_span), op_type, op, full_span) => {
            use ast::OpType::*;
            let lhs_type = check_expr(engine, bindings, lhs_expr)?;
            let rhs_type = check_expr(engine, bindings, rhs_expr)?;

            Ok(match op_type {
                IntOp => {
                    let lhs_bound = engine.int_use(*lhs_span);
                    let rhs_bound = engine.int_use(*rhs_span);
                    engine.flow(lhs_type, lhs_bound)?;
                    engine.flow(rhs_type, rhs_bound)?;
                    engine.int(*full_span)
                }
                FloatOp => {
                    let lhs_bound = engine.float_use(*lhs_span);
                    let rhs_bound = engine.float_use(*rhs_span);
                    engine.flow(lhs_type, lhs_bound)?;
                    engine.flow(rhs_type, rhs_bound)?;
                    engine.float(*full_span)
                }
                StrOp => {
                    let lhs_bound = engine.str_use(*lhs_span);
                    let rhs_bound = engine.str_use(*rhs_span);
                    engine.flow(lhs_type, lhs_bound)?;
                    engine.flow(rhs_type, rhs_bound)?;
                    engine.str(*full_span)
                }
                IntCmp => {
                    let lhs_bound = engine.int_use(*lhs_span);
                    let rhs_bound = engine.int_use(*rhs_span);
                    engine.flow(lhs_type, lhs_bound)?;
                    engine.flow(rhs_type, rhs_bound)?;
                    engine.bool(*full_span)
                }
                FloatCmp => {
                    let lhs_bound = engine.float_use(*lhs_span);
                    let rhs_bound = engine.float_use(*rhs_span);
                    engine.flow(lhs_type, lhs_bound)?;
                    engine.flow(rhs_type, rhs_bound)?;
                    engine.bool(*full_span)
                }
                AnyCmp => engine.bool(*full_span),
            })
        }
        Call(func_expr, arg_expr, span) => {
            let func_type = check_expr(engine, bindings, func_expr)?;
            let arg_type = check_expr(engine, bindings, arg_expr)?;

            let (ret_type, ret_bound) = engine.var();
            let bound = engine.func_use(arg_type, ret_bound, *span);
            engine.flow(func_type, bound)?;
            Ok(ret_type)
        }
        Case((tag, span), val_expr) => {
            let val_type = check_expr(engine, bindings, val_expr)?;
            Ok(engine.case((tag.clone(), val_type), *span))
        }
        FieldAccess(lhs_expr, name, span) => {
            let lhs_type = check_expr(engine, bindings, lhs_expr)?;

            let (field_type, field_bound) = engine.var();
            let bound = engine.obj_use((name.clone(), field_bound), *span);
            engine.flow(lhs_type, bound)?;
            Ok(field_type)
        }
        FuncDef(((arg_name, body_expr), span)) => {
            let (arg_type, arg_bound) = engine.var();
            let body_type = bindings.in_child_scope(|bindings| {
                bindings.insert(arg_name.clone(), arg_type);
                check_expr(engine, bindings, body_expr)
            })?;
            Ok(engine.func(arg_bound, body_type, *span))
        }
        If((cond_expr, span), then_expr, else_expr) => {
            let cond_type = check_expr(engine, bindings, cond_expr)?;
            let bound = engine.bool_use(*span);
            engine.flow(cond_type, bound)?;

            let then_type = check_expr(engine, bindings, then_expr)?;
            let else_type = check_expr(engine, bindings, else_expr)?;

            let (merged, merged_bound) = engine.var();
            engine.flow(then_type, merged_bound)?;
            engine.flow(else_type, merged_bound)?;
            Ok(merged)
        }
        Let((name, var_expr), rest_expr) => {
            let var_scheme = check_let(engine, bindings, var_expr)?;
            bindings.in_child_scope(|bindings| {
                bindings.insert_scheme(name.clone(), var_scheme);
                check_expr(engine, bindings, rest_expr)
            })
        }
        LetRec(defs, rest_expr) => bindings.in_child_scope(|bindings| {
            check_let_rec_defs(engine, bindings, defs)?;
            check_expr(engine, bindings, rest_expr)
        }),
        Literal(type_, (code, span)) => {
            use ast::Literal::*;
            let span = *span;
            Ok(match type_ {
                Bool => engine.bool(span),
                Float => engine.float(span),
                Int => engine.int(span),
                Str => engine.str(span),
            })
        }
        Match(match_expr, cases, span) => {
            let match_type = check_expr(engine, bindings, match_expr)?;
            let (result_type, result_bound) = engine.var();

            // Result types from the match arms
            let mut case_type_pairs = Vec::with_capacity(cases.len());
            let mut wildcard_type = None;

            // Pattern reachability checking
            let mut case_names = HashMap::with_capacity(cases.len());
            let mut wildcard = None;

            for ((pattern, pattern_span), rhs_expr) in cases {
                if let Some(old_span) = wildcard {
                    return Err(SyntaxError::new2(
                        "SyntaxError: Unreachable match pattern",
                        *pattern_span,
                        "Note: Unreachable due to previous wildcard pattern here",
                        old_span,
                    ));
                }

                use ast::Pattern::*;
                match pattern {
                    Case(tag, name) => {
                        if let Some(old_span) = case_names.insert(&*tag, *pattern_span) {
                            return Err(SyntaxError::new2(
                                "SyntaxError: Unreachable match pattern",
                                *pattern_span,
                                "Note: Unreachable due to previous case pattern here",
                                old_span,
                            ));
                        }

                        let (wrapped_type, wrapped_bound) = engine.var();
                        case_type_pairs.push((tag.clone(), wrapped_bound));

                        let rhs_type = bindings.in_child_scope(|bindings| {
                            bindings.insert(name.clone(), wrapped_type);
                            check_expr(engine, bindings, rhs_expr)
                        })?;
                        engine.flow(rhs_type, result_bound)?;
                    }
                    Wildcard(name) => {
                        wildcard = Some(*pattern_span);

                        let (wrapped_type, wrapped_bound) = engine.var();
                        wildcard_type = Some(wrapped_bound);

                        let rhs_type = bindings.in_child_scope(|bindings| {
                            bindings.insert(name.clone(), wrapped_type);
                            check_expr(engine, bindings, rhs_expr)
                        })?;
                        engine.flow(rhs_type, result_bound)?;
                    }
                }
            }

            let bound = engine.case_use(case_type_pairs, wildcard_type, *span);
            engine.flow(match_type, bound)?;

            Ok(result_type)
        }
        NewRef(expr, span) => {
            let expr_type = check_expr(engine, bindings, expr)?;
            let (read, write) = engine.var();
            engine.flow(expr_type, write)?;
            Ok(engine.reference(Some(write), Some(read), *span))
        }
        Record(proto, fields, span) => {
            let proto_type = match proto {
                Some(expr) => Some(check_expr(engine, bindings, expr)?),
                None => None,
            };

            let mut field_names = HashMap::with_capacity(fields.len());
            let mut field_type_pairs = Vec::with_capacity(fields.len());
            for ((name, name_span), expr) in fields {
                if let Some(old_span) = field_names.insert(&*name, *name_span) {
                    return Err(SyntaxError::new2(
                        "SyntaxError: Repeated field name",
                        *name_span,
                        "Note: Field was already defined here",
                        old_span,
                    ));
                }

                let t = check_expr(engine, bindings, expr)?;
                field_type_pairs.push((name.clone(), t));
            }
            Ok(engine.obj(field_type_pairs, proto_type, *span))
        }
        RefGet((expr, span)) => {
            let expr_type = check_expr(engine, bindings, expr)?;

            let (cell_type, cell_bound) = engine.var();
            let bound = engine.reference_use(None, Some(cell_bound), *span);
            engine.flow(expr_type, bound)?;
            Ok(cell_type)
        }
        RefSet((lhs_expr, lhs_span), rhs_expr) => {
            let lhs_type = check_expr(engine, bindings, lhs_expr)?;
            let rhs_type = check_expr(engine, bindings, rhs_expr)?;

            let bound = engine.reference_use(Some(rhs_type), None, *lhs_span);
            engine.flow(lhs_type, bound)?;
            Ok(rhs_type)
        }
        Variable((name, span)) => {
            if let Some(scheme) = bindings.get(name.as_str()) {
                match scheme {
                    Scheme::Mono(v) => Ok(*v),
                    Scheme::Poly(cb) => cb(engine),
                }
            } else {
                Err(SyntaxError::new1(format!("SyntaxError: Undefined variable {}", name), *span))
            }
        }
    }
}

fn check_let(engine: &mut TypeCheckerCore, bindings: &mut Bindings, expr: &ast::Expr) -> Result<Scheme> {
    if let ast::Expr::FuncDef(..) = expr {
        let saved_bindings = RefCell::new(Bindings {
            m: bindings.m.clone(),
            changes: Vec::new(),
        });
        let saved_expr = expr.clone();

        let f: Rc<dyn Fn(&mut TypeCheckerCore) -> Result<Value>> =
            Rc::new(move |engine| check_expr(engine, &mut saved_bindings.borrow_mut(), &saved_expr));

        f(engine)?;
        Ok(Scheme::Poly(f))
    } else {
        let var_type = check_expr(engine, bindings, expr)?;
        Ok(Scheme::Mono(var_type))
    }
}

fn check_let_rec_defs(
    engine: &mut TypeCheckerCore,
    bindings: &mut Bindings,
    defs: &Vec<(String, Box<ast::Expr>)>,
) -> Result<()> {
    let saved_bindings = RefCell::new(Bindings {
        m: bindings.m.clone(),
        changes: Vec::new(),
    });
    let saved_defs = defs.clone();

    let f: Rc<dyn Fn(&mut TypeCheckerCore, usize) -> Result<Value>> = Rc::new(move |engine, i| {
        saved_bindings.borrow_mut().in_child_scope(|bindings| {
            let mut temp_vars = Vec::with_capacity(saved_defs.len());
            for (name, _) in &saved_defs {
                let (temp_type, temp_bound) = engine.var();
                bindings.insert(name.clone(), temp_type);
                temp_vars.push((temp_type, temp_bound));
            }

            for ((_, expr), (_, bound)) in saved_defs.iter().zip(&temp_vars) {
                let var_type = check_expr(engine, bindings, expr)?;
                engine.flow(var_type, *bound)?;
            }

            Ok(temp_vars[i].0)
        })
    });
    f(engine, 0)?;

    for (i, (name, _)) in defs.iter().enumerate() {
        let f = f.clone();
        let scheme = Scheme::Poly(Rc::new(move |engine| f(engine, i)));
        bindings.insert_scheme(name.clone(), scheme);
    }
    Ok(())
}

fn check_toplevel(engine: &mut TypeCheckerCore, bindings: &mut Bindings, def: &ast::TopLevel) -> Result<()> {
    use ast::TopLevel::*;
    match def {
        Expr(expr) => {
            check_expr(engine, bindings, expr)?;
        }
        LetDef((name, var_expr)) => {
            let var_scheme = check_let(engine, bindings, var_expr)?;
            bindings.insert_scheme(name.clone(), var_scheme);
        }
        LetRecDef(defs) => {
            check_let_rec_defs(engine, bindings, defs)?;
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
        let temp = self.core.save();

        for item in parsed {
            if let Err(e) = check_toplevel(&mut self.core, &mut self.bindings, item) {
                // Roll back changes to the type state and bindings
                self.core.restore(temp);
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
