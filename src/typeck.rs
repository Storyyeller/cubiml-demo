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
    PolyLet(Rc<RefCell<PolyLet>>),
    PolyLetRec(Rc<RefCell<PolyLetRec>>, usize),
}

struct PolyLet {
    saved_bindings: Bindings,
    saved_expr: ast::Expr,
    cached: Option<Value>,
}
impl PolyLet {
    fn new(saved_bindings: Bindings, saved_expr: ast::Expr, engine: &mut TypeCheckerCore) -> Result<Self> {
        let mut s = Self {
            saved_bindings,
            saved_expr,
            cached: None,
        };
        s.cached = Some(s.check(engine)?);
        Ok(s)
    }

    fn check(&mut self, engine: &mut TypeCheckerCore) -> Result<Value> {
        if let Some(v) = self.cached.take() {
            return Ok(v);
        }
        check_expr(engine, &mut self.saved_bindings, &self.saved_expr)
    }
}

struct PolyLetRec {
    saved_bindings: Bindings,
    saved_defs: Vec<(String, Box<ast::Expr>)>,
    cached: Option<Vec<(Value, Use)>>,
}
impl PolyLetRec {
    fn new(
        saved_bindings: Bindings,
        saved_defs: Vec<(String, Box<ast::Expr>)>,
        engine: &mut TypeCheckerCore,
    ) -> Result<Self> {
        let mut s = Self {
            saved_bindings,
            saved_defs,
            cached: None,
        };
        s.cached = Some(s.check(engine)?);
        Ok(s)
    }

    fn check(&mut self, engine: &mut TypeCheckerCore) -> Result<Vec<(Value, Use)>> {
        if let Some(v) = self.cached.take() {
            return Ok(v);
        }

        let saved_defs = &self.saved_defs;
        self.saved_bindings.in_child_scope(|bindings| {
            let mut temp_vars = Vec::with_capacity(saved_defs.len());
            for (name, _) in saved_defs.iter() {
                let (temp_type, temp_bound) = engine.var();
                bindings.insert(name.clone(), temp_type);
                temp_vars.push((temp_type, temp_bound));
            }

            for ((_, expr), (_, bound)) in saved_defs.iter().zip(&temp_vars) {
                let var_type = check_expr(engine, bindings, expr)?;
                engine.flow(var_type, *bound)?;
            }

            Ok(temp_vars)
        })
    }
}

struct UnwindPoint(usize);
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

    fn unwind_point(&mut self) -> UnwindPoint {
        UnwindPoint(self.changes.len())
    }

    fn unwind(&mut self, n: UnwindPoint) {
        let n = n.0;
        while self.changes.len() > n {
            let (k, old) = self.changes.pop().unwrap();
            match old {
                Some(v) => self.m.insert(k, v),
                None => self.m.remove(&k),
            };
        }
    }

    fn in_child_scope<T>(&mut self, cb: impl FnOnce(&mut Self) -> T) -> T {
        let n = self.unwind_point();
        let res = cb(self);
        self.unwind(n);
        res
    }
}

struct TypeVarBindings {
    level: u32,
    // Only valid if u32 <= self.level
    m: HashMap<String, ((Value, Use), u32, Span)>,
}
impl TypeVarBindings {
    fn new() -> Self {
        Self {
            level: 0,
            m: HashMap::new(),
        }
    }

    fn insert(&mut self, name: String, v: (Value, Use), span: Span) -> Option<Span> {
        self.m.insert(name, (v, self.level + 1, span)).map(|t| t.2)
    }
}

fn parse_type(engine: &mut TypeCheckerCore, bindings: &mut TypeVarBindings, tyexpr: &ast::TypeExpr) -> Result<(Value, Use)> {
    use ast::TypeExpr::*;
    match tyexpr {
        Alias(lhs, (name, span)) => {
            let (utype_value, utype) = engine.var();
            let (vtype, vtype_bound) = engine.var();

            let old = bindings.insert(name.to_string(), (vtype, utype), *span);
            if let Some(old_span) = old {
                return Err(SyntaxError::new2(
                    format!("SyntaxError: Redefinition of type variable '{}", name),
                    *span,
                    "Note: Type variable was already defined here",
                    old_span,
                ));
            }

            let lhs_type = parse_type(engine, bindings, lhs)?;
            engine.flow(lhs_type.0, vtype_bound)?;
            engine.flow(utype_value, lhs_type.1)?;
            // Make alias permanent by setting level to 0 now that definition is complete
            bindings.m.get_mut(name).unwrap().1 = 0;
            Ok((vtype, utype))
        }
        Case(ext, cases, span) => {
            // Create a dummy variable to use as the lazy flow values
            let dummy = engine.var();
            let (vtype, vtype_bound) = engine.var();

            let utype_wildcard = if let Some(ext) = ext {
                let ext_type = parse_type(engine, bindings, ext)?;
                engine.flow(ext_type.0, vtype_bound)?;
                Some((ext_type.1, dummy))
            } else {
                None
            };

            // Must do this *after* parsing wildcard as wildcards are unguarded
            bindings.level += 1;
            let mut utype_case_arms = Vec::new();
            for ((tag, tag_span), wrapped_expr) in cases {
                let wrapped_type = parse_type(engine, bindings, wrapped_expr)?;

                let case_value = engine.case((tag.clone(), wrapped_type.0), *tag_span);
                engine.flow(case_value, vtype_bound)?;
                utype_case_arms.push((tag.clone(), (wrapped_type.1, dummy)));
            }
            bindings.level -= 1;

            let utype = engine.case_use(utype_case_arms, utype_wildcard, *span);
            Ok((vtype, utype))
        }
        Func(((lhs, rhs), span)) => {
            bindings.level += 1;
            let lhs_type = parse_type(engine, bindings, lhs)?;
            let rhs_type = parse_type(engine, bindings, rhs)?;
            bindings.level -= 1;

            let utype = engine.func_use(lhs_type.0, rhs_type.1, *span);
            let vtype = engine.func(lhs_type.1, rhs_type.0, *span);
            Ok((vtype, utype))
        }
        Ident((s, span)) => match s.as_str() {
            "bool" => Ok((engine.bool(*span), engine.bool_use(*span))),
            "float" => Ok((engine.float(*span), engine.float_use(*span))),
            "int" => Ok((engine.int(*span), engine.int_use(*span))),
            "null" => Ok((engine.null(*span), engine.null_use(*span))),
            "str" => Ok((engine.str(*span), engine.str_use(*span))),
            "number" => {
                let (vtype, vtype_bound) = engine.var();
                let float_lit = engine.float(*span);
                let int_lit = engine.int(*span);
                engine.flow(float_lit, vtype_bound)?;
                engine.flow(int_lit, vtype_bound)?;
                Ok((vtype, engine.int_or_float_use(*span)))
            }
            "top" => {
                let (_, utype) = engine.var();
                let (vtype, vtype_bound) = engine.var();
                let float_lit = engine.float(*span);
                let bool_lit = engine.bool(*span);
                engine.flow(float_lit, vtype_bound)?;
                engine.flow(bool_lit, vtype_bound)?;
                Ok((vtype, utype))
            }
            "bot" => {
                let (vtype, _) = engine.var();
                let (utype_value, utype) = engine.var();
                let float_lit = engine.float_use(*span);
                let bool_lit = engine.bool_use(*span);
                engine.flow(utype_value, float_lit)?;
                engine.flow(utype_value, bool_lit)?;
                Ok((vtype, utype))
            }
            "_" => Ok(engine.var()),
            _ => Err(SyntaxError::new1(
                "SyntaxError: Unrecognized simple type (choices are bool, float, int, str, number, null, top, bot, or _)",
                *span,
            )),
        },
        Nullable(lhs, span) => {
            let lhs_type = parse_type(engine, bindings, lhs)?;
            let utype = engine.null_check_use(lhs_type.1, *span);

            let (vtype, vtype_bound) = engine.var();
            let null_lit = engine.null(*span);
            engine.flow(lhs_type.0, vtype_bound)?;
            engine.flow(null_lit, vtype_bound)?;
            Ok((vtype, utype))
        }
        Record(ext, fields, span) => {
            let (utype_value, utype) = engine.var();

            let vtype_wildcard = if let Some(ext) = ext {
                let ext_type = parse_type(engine, bindings, ext)?;
                engine.flow(utype_value, ext_type.1)?;
                Some(ext_type.0)
            } else {
                None
            };

            // Must do this *after* parsing wildcard as wildcards are unguarded
            bindings.level += 1;
            let mut vtype_fields = Vec::new();
            for ((name, name_span), wrapped_expr) in fields {
                let wrapped_type = parse_type(engine, bindings, wrapped_expr)?;

                let obj_use = engine.obj_use((name.clone(), wrapped_type.1), *name_span);
                engine.flow(utype_value, obj_use)?;
                vtype_fields.push((name.clone(), wrapped_type.0));
            }
            bindings.level -= 1;

            let vtype = engine.obj(vtype_fields, vtype_wildcard, *span);
            Ok((vtype, utype))
        }
        Ref(lhs, (rw, span)) => {
            use ast::Readability::*;
            bindings.level += 1;
            let lhs_type = parse_type(engine, bindings, lhs)?;
            bindings.level -= 1;

            let write = if *rw == ReadOnly {
                (None, None)
            } else {
                (Some(lhs_type.1), Some(lhs_type.0))
            };
            let read = if *rw == WriteOnly {
                (None, None)
            } else {
                (Some(lhs_type.0), Some(lhs_type.1))
            };

            let vtype = engine.reference(write.0, read.0, *span);
            let utype = engine.reference_use(write.1, read.1, *span);
            Ok((vtype, utype))
        }
        TypeVar((name, span)) => {
            if let Some((res, lvl, _)) = bindings.m.get(name.as_str()).copied() {
                if lvl <= bindings.level {
                    Ok(res)
                } else {
                    Err(SyntaxError::new1(
                    format!("SyntaxError: Unguarded type variable {}. Recursive type variables must be nested within a case, record field, function, or ref", name),
                    *span))
                }
            } else {
                Err(SyntaxError::new1(
                    format!("SyntaxError: Undefined type variable {}", name),
                    *span,
                ))
            }
        }
    }
}

fn parse_type_signature(engine: &mut TypeCheckerCore, tyexpr: &ast::TypeExpr) -> Result<(Value, Use)> {
    let mut bindings = TypeVarBindings::new();
    parse_type(engine, &mut bindings, tyexpr)
}

fn process_let_pattern(engine: &mut TypeCheckerCore, bindings: &mut Bindings, pat: &ast::LetPattern) -> Result<Use> {
    use ast::LetPattern::*;

    let (arg_type, arg_bound) = engine.var();
    match pat {
        Var(name) => {
            bindings.insert(name.clone(), arg_type);
        }
        Record(pairs) => {
            let mut field_names = HashMap::with_capacity(pairs.len());

            for ((name, name_span), sub_pattern) in pairs {
                if let Some(old_span) = field_names.insert(&*name, *name_span) {
                    return Err(SyntaxError::new2(
                        "SyntaxError: Repeated field pattern name",
                        *name_span,
                        "Note: Field was already bound here",
                        old_span,
                    ));
                }

                let field_bound = process_let_pattern(engine, bindings, &*sub_pattern)?;
                let bound = engine.obj_use((name.clone(), field_bound), *name_span);
                engine.flow(arg_type, bound)?;
            }
        }
    };
    Ok(arg_bound)
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
                IntOrFloatCmp => {
                    let lhs_bound = engine.int_or_float_use(*lhs_span);
                    let rhs_bound = engine.int_or_float_use(*rhs_span);
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
        FuncDef(((arg_pattern, body_expr), span)) => {
            let (arg_bound, body_type) = bindings.in_child_scope(|bindings| {
                let arg_bound = process_let_pattern(engine, bindings, arg_pattern)?;
                let body_type = check_expr(engine, bindings, body_expr)?;
                Ok((arg_bound, body_type))
            })?;
            Ok(engine.func(arg_bound, body_type, *span))
        }
        If((cond_expr, span), then_expr, else_expr) => {
            // Handle conditions of the form foo == null and foo != null specially
            if let BinOp((lhs, _), (rhs, _), ast::OpType::AnyCmp, op, ..) = &**cond_expr {
                if let Variable((name, _)) = &**lhs {
                    if let Literal(ast::Literal::Null, ..) = **rhs {
                        if let Some(scheme) = bindings.get(name.as_str()) {
                            if let Scheme::Mono(lhs_type) = scheme {
                                // Flip order of branches if they wrote if foo == null instead of !=
                                let (ok_expr, else_expr) = match op {
                                    ast::Op::Neq => (then_expr, else_expr),
                                    ast::Op::Eq => (else_expr, then_expr),
                                    _ => unreachable!(),
                                };

                                let (nnvar_type, nnvar_bound) = engine.var();
                                let bound = engine.null_check_use(nnvar_bound, *span);
                                engine.flow(*lhs_type, bound)?;

                                let ok_type = bindings.in_child_scope(|bindings| {
                                    bindings.insert(name.clone(), nnvar_type);
                                    check_expr(engine, bindings, ok_expr)
                                })?;
                                let else_type = check_expr(engine, bindings, else_expr)?;

                                let (merged, merged_bound) = engine.var();
                                engine.flow(ok_type, merged_bound)?;
                                engine.flow(else_type, merged_bound)?;
                                return Ok(merged);
                            }
                        }
                    }
                }
            }

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
        Let((pattern, var_expr), rest_expr) => {
            let mark = bindings.unwind_point();

            check_let_def(engine, bindings, pattern, var_expr)?;
            let result_type = check_expr(engine, bindings, rest_expr)?;

            bindings.unwind(mark);
            Ok(result_type)
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
                Null => engine.null(span),
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

                use ast::MatchPattern::*;
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
                        let rhs_type = bindings.in_child_scope(|bindings| {
                            bindings.insert(name.clone(), wrapped_type);
                            check_expr(engine, bindings, rhs_expr)
                        })?;

                        case_type_pairs.push((tag.clone(), (wrapped_bound, (rhs_type, result_bound))));
                    }
                    Wildcard(name) => {
                        wildcard = Some(*pattern_span);

                        let (wrapped_type, wrapped_bound) = engine.var();
                        let rhs_type = bindings.in_child_scope(|bindings| {
                            bindings.insert(name.clone(), wrapped_type);
                            check_expr(engine, bindings, rhs_expr)
                        })?;

                        wildcard_type = Some((wrapped_bound, (rhs_type, result_bound)));
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
        Println(args, rest_expr) => {
            for arg in args {
                check_expr(engine, bindings, arg)?;
            }
            check_expr(engine, bindings, rest_expr)
        }
        Seq(lhs_expr, rhs_expr) => {
            let _lhs_type = check_expr(engine, bindings, lhs_expr)?;
            let rhs_type = check_expr(engine, bindings, rhs_expr)?;
            Ok(rhs_type)
        }
        Typed(expr, sig) => {
            let expr_type = check_expr(engine, bindings, expr)?;
            let sig_type = parse_type_signature(engine, sig)?;
            engine.flow(expr_type, sig_type.1)?;
            Ok(sig_type.0)
        }
        Variable((name, span)) => {
            if let Some(scheme) = bindings.get(name.as_str()) {
                match scheme {
                    Scheme::Mono(v) => Ok(*v),
                    Scheme::PolyLet(cb) => cb.borrow_mut().check(engine),
                    Scheme::PolyLetRec(cb, i) => Ok(cb.borrow_mut().check(engine)?[*i].0),
                }
            } else {
                Err(SyntaxError::new1(format!("SyntaxError: Undefined variable {}", name), *span))
            }
        }
    }
}

fn check_let_def(
    engine: &mut TypeCheckerCore,
    bindings: &mut Bindings,
    lhs: &ast::LetPattern,
    expr: &ast::Expr,
) -> Result<()> {
    use ast::LetPattern::*;
    if let ast::Expr::FuncDef((_, span)) = expr {
        let name = match lhs {
            Var(name) => name.to_owned(),
            _ => return Err(SyntaxError::new1(format!("TypeError: Cannot destructure function"), *span)),
        };

        let saved_bindings = Bindings {
            m: bindings.m.clone(),
            changes: Vec::new(),
        };
        let saved_expr = expr.clone();

        let f = PolyLet::new(saved_bindings, saved_expr, engine)?;
        bindings.insert_scheme(name, Scheme::PolyLet(Rc::new(RefCell::new(f))));
    } else {
        let var_type = check_expr(engine, bindings, expr)?;
        let bound = process_let_pattern(engine, bindings, lhs)?;
        engine.flow(var_type, bound)?;
    }
    Ok(())
}

fn check_let_rec_defs(
    engine: &mut TypeCheckerCore,
    bindings: &mut Bindings,
    defs: &Vec<(String, Box<ast::Expr>)>,
) -> Result<()> {
    let saved_bindings = Bindings {
        m: bindings.m.clone(),
        changes: Vec::new(),
    };
    let saved_defs = defs.clone();

    let f = PolyLetRec::new(saved_bindings, saved_defs, engine)?;
    let f = Rc::new(RefCell::new(f));

    for (i, (name, _)) in defs.iter().enumerate() {
        bindings.insert_scheme(name.clone(), Scheme::PolyLetRec(f.clone(), i));
    }
    Ok(())
}

fn check_toplevel(engine: &mut TypeCheckerCore, bindings: &mut Bindings, def: &ast::TopLevel) -> Result<()> {
    use ast::TopLevel::*;
    match def {
        Empty => {}
        Expr(expr) => {
            check_expr(engine, bindings, expr)?;
        }
        LetDef((pattern, var_expr)) => {
            check_let_def(engine, bindings, pattern, var_expr)?;
        }
        LetRecDef(defs) => {
            check_let_rec_defs(engine, bindings, defs)?;
        }
        Println(exprs) => {
            for expr in exprs {
                check_expr(engine, bindings, expr)?;
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
        let temp = self.core.save();
        assert!(self.bindings.changes.is_empty());
        let mark = self.bindings.unwind_point();

        for item in parsed {
            if let Err(e) = check_toplevel(&mut self.core, &mut self.bindings, item) {
                // Roll back changes to the type state and bindings
                self.core.restore(temp);
                self.bindings.unwind(mark);
                return Err(e);
            }
        }

        // Now that script type-checked successfully, make the global definitions permanent
        // by removing them from the changes rollback list
        self.bindings.changes.clear();
        Ok(())
    }
}
