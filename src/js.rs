#![allow(dead_code)]

#[derive(Debug, Clone, Copy)]
pub enum Op {
    Add,
    Sub,
    Mult,
    Div,

    Lt,
    Lte,
    Gt,
    Gte,

    Eq,
    Neq,
}

/////////////////////////////////////////////////////////////////////////////////////////////
pub fn assign(lhs: Expr, rhs: Expr) -> Expr {
    Expr(Expr2::Assignment(lhs.0.into(), rhs.0.into()))
}
pub fn binop(lhs: Expr, rhs: Expr, op: Op) -> Expr {
    Expr(Expr2::BinOp(lhs.0.into(), rhs.0.into(), op))
}
pub fn call(lhs: Expr, rhs: Expr) -> Expr {
    Expr(Expr2::Call(lhs.0.into(), rhs.0.into()))
}
pub fn comma_pair(lhs: Expr, rhs: Expr) -> Expr {
    Expr(Expr2::Comma(lhs.0.into(), rhs.0.into()))
}
pub fn unary_minus(rhs: Expr) -> Expr {
    Expr(Expr2::Minus(rhs.0.into()))
}
pub fn eqop(lhs: Expr, rhs: Expr) -> Expr {
    Expr(Expr2::BinOp(lhs.0.into(), rhs.0.into(), Op::Eq))
}
pub fn field(lhs: Expr, rhs: String) -> Expr {
    Expr(Expr2::Field(lhs.0.into(), rhs))
}
pub fn lit(code: String) -> Expr {
    Expr(Expr2::Literal(code))
}
pub fn ternary(cond: Expr, e1: Expr, e2: Expr) -> Expr {
    Expr(Expr2::Ternary(cond.0.into(), e1.0.into(), e2.0.into()))
}
pub fn var(s: String) -> Expr {
    Expr(Expr2::Var(s))
}

pub fn comma_list(mut exprs: Vec<Expr>) -> Expr {
    // Reverse the list so we can easily create a left-recursive structure instead of right recursive
    exprs.reverse();
    let mut res = exprs.pop().unwrap().0;
    while let Some(expr) = exprs.pop() {
        res = Expr2::Comma(Box::new(res), expr.0.into());
    }
    Expr(res)
}

pub fn func(arg: String, scope: String, body: Expr) -> Expr {
    let mut body = body.0;

    // body can't be an expression starting with "{"
    if body.first() == Token::BRACE {
        body.wrap_in_parens();
    }

    Expr(Expr2::ArrowFunc(arg, scope, Box::new(body)))
}

pub fn obj(spread: Option<Expr>, fields: Vec<(String, Expr)>) -> Expr {
    let mut prop_defs = Vec::new();
    for v in spread {
        prop_defs.push(PropertyDefinition::Spread(v.0.into()));
    }
    for (name, v) in fields {
        prop_defs.push(PropertyDefinition::Named(name, v.0.into()));
    }

    Expr(Expr2::Obj(prop_defs))
}

#[derive(Clone, Debug)]
pub struct Expr(Expr2);
impl Expr {
    // pub fn add_parens(&mut self) {
    //     self.0.add_parens();
    // }

    pub fn to_source(mut self) -> String {
        self.0.add_parens();

        let mut s = "".to_string();
        self.0.write(&mut s);
        s
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Token {
    OTHER,
    BRACE,
    PAREN,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    PRIMARY = 0,
    MEMBER,
    CALL,
    LHS,
    UNARY,
    EXPONENT,
    MULTIPLICATIVE,
    ADDITIVE,
    SHIFT,
    RELATIONAL,
    EQUALITY,
    LOR,
    CONDITIONAL,
    ASSIGN,
    EXPR,
}

#[derive(Clone, Debug)]
enum PropertyDefinition {
    Named(String, Box<Expr2>),
    Spread(Box<Expr2>),
}

#[derive(Clone, Debug)]
enum Expr2 {
    Paren(Box<Expr2>),
    Literal(String),
    Obj(Vec<PropertyDefinition>),
    Var(String),

    Field(Box<Expr2>, String),

    Call(Box<Expr2>, Box<Expr2>),

    Minus(Box<Expr2>),

    BinOp(Box<Expr2>, Box<Expr2>, Op),

    Ternary(Box<Expr2>, Box<Expr2>, Box<Expr2>),

    Assignment(Box<Expr2>, Box<Expr2>),
    ArrowFunc(String, String, Box<Expr2>),

    Comma(Box<Expr2>, Box<Expr2>),
}
impl Expr2 {
    fn precedence(&self) -> Precedence {
        use Expr2::*;
        use Op::*;
        use Precedence::*;
        match self {
            Paren(..) => PRIMARY,
            Literal(..) => PRIMARY,
            Obj(..) => PRIMARY,
            Var(..) => PRIMARY,
            Field(..) => MEMBER,
            Call(..) => CALL,
            Minus(..) => UNARY,
            BinOp(_, _, op) => match op {
                Mult | Div => MULTIPLICATIVE,
                Add | Sub => ADDITIVE,
                Lt | Lte | Gt | Gte => RELATIONAL,
                Eq | Neq => EQUALITY,
            },
            Ternary(..) => CONDITIONAL,
            Assignment(..) => ASSIGN,
            ArrowFunc(..) => ASSIGN,
            Comma(..) => EXPR,
        }
    }

    fn first(&self) -> Token {
        use Expr2::*;
        use Token::*;
        match self {
            Paren(..) => PAREN,
            Literal(..) => OTHER,
            Obj(..) => BRACE,
            Var(..) => OTHER,
            Field(lhs, ..) => lhs.first(),
            Call(lhs, ..) => lhs.first(),
            Minus(..) => OTHER,
            BinOp(lhs, ..) => lhs.first(),
            Ternary(lhs, ..) => lhs.first(),
            Assignment(lhs, ..) => lhs.first(),
            ArrowFunc(..) => PAREN,
            Comma(lhs, ..) => lhs.first(),
        }
    }

    fn write(&self, out: &mut String) {
        match self {
            Self::Paren(e) => {
                *out += "(";
                e.write(out);
                *out += ")";
            }
            Self::Literal(code) => {
                *out += code;
            }
            Self::Obj(fields) => {
                *out += "{";
                for prop_def in fields {
                    use PropertyDefinition::*;
                    match prop_def {
                        Named(name, val) => {
                            *out += "'";
                            *out += name;
                            *out += "': ";
                            val.write(out);
                        }
                        Spread(val) => {
                            *out += "...";
                            val.write(out);
                        }
                    }

                    *out += ", ";
                }
                *out += "}";
            }
            Self::Var(name) => {
                *out += name;
            }
            Self::Field(lhs, rhs) => {
                lhs.write(out);
                *out += ".";
                *out += rhs;
            }
            Self::Call(lhs, rhs) => {
                lhs.write(out);
                *out += "(";
                rhs.write(out);
                *out += ")";
            }
            Self::Minus(e) => {
                *out += "-";
                e.write(out);
            }
            Self::BinOp(lhs, rhs, op) => {
                use Op::*;
                let opstr = match op {
                    Add => "+",
                    Sub => "- ",
                    Mult => "*",
                    Div => "/",

                    Lt => "<",
                    Lte => "<=",
                    Gt => ">",
                    Gte => ">=",

                    Eq => "===",
                    Neq => "!==",
                };

                lhs.write(out);
                *out += opstr;
                rhs.write(out);
            }
            Self::Ternary(cond, e1, e2) => {
                cond.write(out);
                *out += " ? ";
                e1.write(out);
                *out += " : ";
                e2.write(out);
            }
            Self::Assignment(lhs, rhs) => {
                lhs.write(out);
                *out += " = ";
                rhs.write(out);
            }
            Self::ArrowFunc(arg, scope_arg, body) => {
                *out += "(";
                *out += arg;
                *out += ", ";
                *out += scope_arg;
                *out += "={}) => ";
                body.write(out);
            }
            Self::Comma(lhs, rhs) => {
                lhs.write(out);
                *out += ", ";
                rhs.write(out);
            }
        }
    }

    fn wrap_in_parens(&mut self) {
        use Expr2::*;
        let dummy = Literal("".to_string());
        let temp = std::mem::replace(self, dummy);
        std::mem::replace(self, Paren(Box::new(temp)));
    }

    fn ensure(&mut self, required: Precedence) {
        if self.precedence() > required {
            self.wrap_in_parens();
        }
    }

    fn add_parens(&mut self) {
        use Precedence::*;
        match self {
            Self::Paren(e) => {
                e.add_parens();
            }
            Self::Literal(code) => {}
            Self::Obj(fields) => {
                for prop_def in fields {
                    use PropertyDefinition::*;
                    match prop_def {
                        Named(name, val) => {
                            val.add_parens();
                            val.ensure(ASSIGN);
                        }
                        Spread(val) => {
                            val.add_parens();
                            val.ensure(ASSIGN);
                        }
                    }
                }
            }
            Self::Var(name) => {}
            Self::Field(lhs, rhs) => {
                lhs.add_parens();
                lhs.ensure(MEMBER);
            }
            Self::Call(lhs, rhs) => {
                lhs.add_parens();
                lhs.ensure(MEMBER);
                rhs.add_parens();
                rhs.ensure(ASSIGN);
            }
            Self::Minus(e) => {
                e.add_parens();
                e.ensure(UNARY);
            }
            Self::BinOp(lhs, rhs, op) => {
                use Op::*;
                let req = match op {
                    Mult | Div => (MULTIPLICATIVE, EXPONENT),
                    Add | Sub => (ADDITIVE, MULTIPLICATIVE),
                    Lt | Lte | Gt | Gte => (RELATIONAL, SHIFT),
                    Eq | Neq => (EQUALITY, RELATIONAL),
                };

                lhs.add_parens();
                lhs.ensure(req.0);
                rhs.add_parens();
                rhs.ensure(req.1);
            }
            Self::Ternary(cond, e1, e2) => {
                cond.add_parens();
                e1.add_parens();
                e1.ensure(ASSIGN);
                e2.add_parens();
                e2.ensure(ASSIGN);
            }
            Self::Assignment(lhs, rhs) => {
                lhs.add_parens();
                lhs.ensure(LHS);
                rhs.add_parens();
                rhs.ensure(ASSIGN);
            }
            Self::ArrowFunc(arg, scope_arg, body) => {
                body.add_parens();
                body.ensure(ASSIGN);
                // body can't be an expression starting with "{"
                if body.first() == Token::BRACE {
                    body.wrap_in_parens();
                }
            }
            Self::Comma(lhs, rhs) => {
                lhs.add_parens();
                rhs.add_parens();
                rhs.ensure(ASSIGN);
            }
        }
    }
}
