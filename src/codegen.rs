use std::collections::HashMap;
use std::mem::swap;

use crate::ast;
use crate::js;

pub struct ModuleBuilder {
    scope_expr: js::Expr,
    scope_counter: u64,
    param_counter: u64,
    var_counter: u64,                    // For choosing new var names
    bindings: HashMap<String, js::Expr>, // ML name -> JS exor for current scope
    changes: Vec<(String, Option<js::Expr>)>,
}
impl ModuleBuilder {
    pub fn new() -> Self {
        Self {
            scope_expr: js::var("$".to_string()),
            scope_counter: 0,
            param_counter: 0,
            var_counter: 0,
            bindings: HashMap::new(),
            changes: Vec::new(),
        }
    }

    pub fn compile_script(&mut self, def: &[ast::TopLevel]) -> js::Expr {
        compile_script(self, def)
    }

    fn ml_scope<T>(&mut self, cb: impl FnOnce(&mut Self) -> T) -> T {
        let n = self.changes.len();
        let res = cb(self);
        while self.changes.len() > n {
            let (k, old) = self.changes.pop().unwrap();
            match old {
                Some(v) => self.bindings.insert(k, v),
                None => self.bindings.remove(&k),
            };
        }
        res
    }

    fn fn_scope<T>(&mut self, cb: impl FnOnce(&mut Self) -> T) -> T {
        let old_var_counter = self.var_counter;
        let old_param_counter = self.param_counter;
        let old_scope_counter = self.scope_counter;
        self.var_counter = 0;

        let res = self.ml_scope(cb);

        self.var_counter = old_var_counter;
        self.param_counter = old_param_counter;
        self.scope_counter = old_scope_counter;
        res
    }

    fn set_binding(&mut self, k: String, v: js::Expr) {
        let old = self.bindings.insert(k.clone(), v);
        self.changes.push((k, old));
    }

    fn new_var_name(&mut self) -> String {
        let js_name = format!("v{}", self.var_counter);
        self.var_counter += 1;
        js_name
    }

    fn new_var(&mut self, ml_name: &str) -> js::Expr {
        let js_name = self.new_var_name();
        let expr = js::field(self.scope_expr.clone(), js_name);
        self.set_binding(ml_name.to_string(), expr.clone());
        expr
    }

    fn new_scope_name(&mut self) -> String {
        let js_name = format!("s{}", self.scope_counter);
        self.scope_counter += 1;
        js_name
    }

    fn new_param_name(&mut self) -> String {
        let js_name = format!("p{}", self.param_counter);
        self.param_counter += 1;
        js_name
    }
}

fn compile(ctx: &mut ModuleBuilder, expr: &ast::Expr) -> js::Expr {
    match expr {
        ast::Expr::BinOp((lhs_expr, lhs_span), (rhs_expr, rhs_span), op_type, op, full_span) => {
            let lhs = compile(ctx, lhs_expr);
            let rhs = compile(ctx, rhs_expr);
            let jsop = match op {
                ast::Op::Add => js::Op::Add,
                ast::Op::Sub => js::Op::Sub,
                ast::Op::Mult => js::Op::Mult,
                ast::Op::Div => js::Op::Div,
                ast::Op::Rem => js::Op::Rem,

                ast::Op::Lt => js::Op::Lt,
                ast::Op::Lte => js::Op::Lte,
                ast::Op::Gt => js::Op::Gt,
                ast::Op::Gte => js::Op::Gte,

                ast::Op::Eq => js::Op::Eq,
                ast::Op::Neq => js::Op::Neq,
            };
            js::binop(lhs, rhs, jsop)
        }
        ast::Expr::Call(func, arg, _) => {
            let lhs = compile(ctx, func);
            let rhs = compile(ctx, arg);
            js::call(lhs, rhs)
        }
        ast::Expr::Case((tag, _), expr) => {
            let tag = js::lit(format!("\"{}\"", tag));
            let expr = compile(ctx, expr);
            js::obj(None, vec![("$tag".to_string(), tag), ("$val".to_string(), expr)])
        }
        ast::Expr::FieldAccess(lhs_expr, name, _) => {
            let lhs = compile(ctx, lhs_expr);
            js::field(lhs, name.clone())
        }
        ast::Expr::FuncDef(((arg_pattern, body_expr), _)) => {
            ctx.fn_scope(|ctx| {
                let new_scope_name = ctx.new_scope_name();
                let mut scope_expr = js::var(new_scope_name.clone());
                swap(&mut scope_expr, &mut ctx.scope_expr);

                //////////////////////////////////////////////////////
                let js_pattern = compile_let_pattern(ctx, arg_pattern);
                let body = compile(ctx, body_expr);
                //////////////////////////////////////////////////////

                swap(&mut scope_expr, &mut ctx.scope_expr);
                js::func(js_pattern, new_scope_name, body)
            })
        }
        ast::Expr::If((cond_expr, _), then_expr, else_expr) => {
            let cond_expr = compile(ctx, cond_expr);
            let then_expr = compile(ctx, then_expr);
            let else_expr = compile(ctx, else_expr);
            js::ternary(cond_expr, then_expr, else_expr)
        }
        ast::Expr::Let((name, var_expr), rest_expr) => {
            let rhs = compile(ctx, var_expr);

            ctx.ml_scope(|ctx| {
                let lhs = ctx.new_var(name);
                let rest = compile(ctx, rest_expr);
                js::comma_pair(js::assign(lhs, rhs), rest)
            })
        }
        ast::Expr::LetRec(defs, rest_expr) => {
            ctx.ml_scope(|ctx| {
                // let temp = defs.iter().map(|(name, _)| ctx.new_var(name)).collect::<Vec<_>>();
                let mut vars = Vec::new();
                let mut exprs = Vec::new();
                for (name, _) in defs {
                    vars.push(ctx.new_var(name))
                }
                for (_, expr) in defs {
                    exprs.push(compile(ctx, expr))
                }

                let mut exprs = vars
                    .into_iter()
                    .zip(exprs)
                    .map(|(lhs, rhs)| js::assign(lhs, rhs))
                    .collect::<Vec<_>>();
                exprs.push(compile(ctx, rest_expr));
                js::comma_list(exprs)
            })
        }
        ast::Expr::Literal(type_, (code, _)) => {
            let mut code = code.clone();
            if let ast::Literal::Int = type_ {
                code.push_str("n");
            }
            if code.starts_with("-") {
                js::unary_minus(js::lit(code[1..].to_string()))
            } else {
                js::lit(code)
            }
        }
        ast::Expr::Match(match_expr, cases, _) => {
            let temp_var = js::field(ctx.scope_expr.clone(), ctx.new_var_name());
            let part1 = js::assign(temp_var.clone(), compile(ctx, match_expr));

            let tag_expr = js::field(temp_var.clone(), "$tag".to_string());
            let val_expr = js::field(temp_var.clone(), "$val".to_string());

            let mut branches = Vec::new();
            for ((pattern, _), rhs_expr) in cases {
                use ast::MatchPattern::*;
                match pattern {
                    Case(tag, name) => {
                        ctx.ml_scope(|ctx| {
                            ctx.set_binding(name.to_string(), val_expr.clone());
                            branches.push((tag.as_str(), compile(ctx, rhs_expr)));
                        });
                    }
                    Wildcard(name) => {
                        ctx.ml_scope(|ctx| {
                            ctx.set_binding(name.to_string(), temp_var.clone());
                            branches.push(("", compile(ctx, rhs_expr)));
                        });
                    }
                }
            }

            let mut res = branches.pop().unwrap().1;
            while let Some((tag, rhs_expr)) = branches.pop() {
                assert!(tag.len() > 0);
                let cond = js::eqop(tag_expr.clone(), js::lit(format!("\"{}\"", tag)));
                res = js::ternary(cond, rhs_expr, res);
            }
            js::comma_pair(part1, res)
        }
        ast::Expr::NewRef(expr, span) => {
            let expr = compile(ctx, expr);
            js::obj(None, vec![("$p".to_string(), expr)])
        }
        ast::Expr::Record(proto, fields, span) => js::obj(
            proto.as_ref().map(|expr| compile(ctx, expr)),
            fields
                .iter()
                .map(|((name, _), expr)| (name.clone(), compile(ctx, expr)))
                .collect(),
        ),
        ast::Expr::RefGet((expr, span)) => {
            let expr = compile(ctx, expr);
            js::field(expr, "$p".to_string())
        }
        ast::Expr::RefSet((lhs_expr, lhs_span), rhs_expr) => {
            let lhs = compile(ctx, lhs_expr);
            let rhs = compile(ctx, rhs_expr);
            js::assign(js::field(lhs, "$p".to_string()), rhs)
        }
        ast::Expr::Seq(lhs_expr, rhs_expr) => {
            let lhs = compile(ctx, lhs_expr);
            let rhs = compile(ctx, rhs_expr);
            js::comma_pair(lhs, rhs)
        }
        ast::Expr::Typed(expr, _) => compile(ctx, expr),
        ast::Expr::Variable((name, _)) => ctx.bindings.get(name).unwrap().clone(),
    }
}

fn compile_let_pattern(ctx: &mut ModuleBuilder, pat: &ast::LetPattern) -> js::Expr {
    use ast::LetPattern::*;
    match pat {
        Var(ml_name) => {
            let js_arg = js::var(ctx.new_param_name());
            ctx.set_binding(ml_name.to_string(), js_arg.clone());
            js_arg
        }
        Record(pairs) => js::obj(
            None,
            pairs
                .iter()
                .map(|((name, _), pat)| (name.clone(), compile_let_pattern(ctx, &*pat)))
                .collect(),
        ),
    }
}

fn compile_script(ctx: &mut ModuleBuilder, parsed: &[ast::TopLevel]) -> js::Expr {
    let mut exprs = Vec::new();

    for item in parsed {
        use ast::TopLevel::*;
        match item {
            Expr(expr) => exprs.push(compile(ctx, expr)),
            LetDef((name, var_expr)) => {
                let rhs = compile(ctx, var_expr);
                let lhs = ctx.new_var(name);
                exprs.push(js::assign(lhs, rhs));
            }
            LetRecDef(defs) => {
                let mut vars = Vec::new();
                let mut rhs_exprs = Vec::new();
                for (name, _) in defs {
                    vars.push(ctx.new_var(name))
                }
                for (_, expr) in defs {
                    rhs_exprs.push(compile(ctx, expr))
                }

                for (lhs, rhs) in vars.into_iter().zip(rhs_exprs) {
                    exprs.push(js::assign(lhs, rhs));
                }
            }
        }
    }

    js::comma_list(exprs)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        use ast::*;

        fn intlit(s: &str) -> Box<ast::Expr> {
            Box::new(Expr::Literal(Literal::Int, s.to_string()))
        }

        let mut mb = ModuleBuilder::new();
        assert_eq!(compile(&mut mb, &intlit("-1")).to_source(), "-1n");
        assert_eq!(
            compile(&mut mb, &Expr::FieldAccess(intlit("-1"), "toString".to_string())).to_source(),
            "(-1n).toString"
        );

        assert_eq!(
            compile(
                &mut mb,
                &Expr::BinOp(intlit("42"), intlit("-1"), ast::OpType::IntOp, ast::Op::Sub)
            )
            .to_source(),
            "42n- -1n"
        );
    }
}
