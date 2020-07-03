#![allow(dead_code)]

#[derive(Clone, Debug)]
pub enum Literal {
    Bool(bool),
    Str(String),
}

#[derive(Clone, Debug)]
enum PrimaryExpr {
    Paren(Box<Expression>),
    Literal(Literal),
    Obj(Vec<(String, Box<AssignExpr>)>),
    Var(String),
}

#[derive(Clone, Debug)]
enum MemberExpr {
    SP(PrimaryExpr),
    Field(Box<MemberExpr>, String),
}

#[derive(Clone, Debug)]
enum CallExpr {
    SM(MemberExpr),
    Call(MemberExpr, Box<AssignExpr>),
}
type LHSExpr = CallExpr;
type RelationalExpr = LHSExpr;

#[derive(Clone, Debug)]
enum EqualityExpr {
    SR(RelationalExpr),
    EqOp(Box<EqualityExpr>, RelationalExpr),
}
type LOrExpr = EqualityExpr;

#[derive(Clone, Debug)]
enum ConditionalExpr {
    SLO(LOrExpr),
    Ternary(LOrExpr, Box<AssignExpr>, Box<AssignExpr>),
}

#[derive(Clone, Debug)]
enum AssignExpr {
    SC(ConditionalExpr),
    Assignment(LHSExpr, Box<AssignExpr>),
    ArrowFunc(String, String, Box<AssignExpr>),
}

#[derive(Clone, Debug)]
enum Expression {
    SA(AssignExpr),
    Comma(Box<Expression>, AssignExpr),
}

#[derive(Clone, Debug)]
pub struct Expr(Expression);

#[derive(Clone, Copy, PartialEq, Eq)]
enum Token {
    OTHER,
    BRACE,
    PAREN,
}

impl PrimaryExpr {
    fn sup(self) -> MemberExpr {
        MemberExpr::SP(self)
    }
    fn expr(self) -> Expr {
        self.sup().expr()
    }

    fn as_member(self) -> MemberExpr {
        self.sup()
    }
    fn as_lhs(self) -> LHSExpr {
        self.as_member().sup()
    }
    fn as_lor(self) -> LOrExpr {
        self.as_lhs().sup()
    }
    fn as_cond(self) -> ConditionalExpr {
        self.as_lor().sup()
    }
    fn as_assign(self) -> AssignExpr {
        self.as_cond().sup()
    }

    fn first(&self) -> Token {
        match self {
            Self::Obj(..) => Token::BRACE,
            Self::Paren(..) => Token::PAREN,
            _ => Token::OTHER,
        }
    }
}
impl MemberExpr {
    fn sup(self) -> CallExpr {
        CallExpr::SM(self)
    }
    fn expr(self) -> Expr {
        self.sup().expr()
    }
    fn as_primary(self) -> PrimaryExpr {
        if let Self::SP(s) = self {
            s
        } else {
            parens(self.expr())
        }
    }

    fn first(&self) -> Token {
        match self {
            Self::SP(s) => s.first(),
            Self::Field(lhs, ..) => lhs.first(),
        }
    }
}
impl RelationalExpr {
    fn sup(self) -> EqualityExpr {
        EqualityExpr::SR(self)
    }
    fn expr(self) -> Expr {
        self.sup().expr()
    }
    fn as_member(self) -> MemberExpr {
        if let Self::SM(s) = self {
            s
        } else {
            parens(self.expr()).as_member()
        }
    }

    fn first(&self) -> Token {
        match self {
            Self::SM(s) => s.first(),
            Self::Call(lhs, ..) => lhs.first(),
        }
    }
}
impl LOrExpr {
    fn sup(self) -> ConditionalExpr {
        ConditionalExpr::SLO(self)
    }
    fn expr(self) -> Expr {
        self.sup().expr()
    }
    fn as_lhs(self) -> LHSExpr {
        if let Self::SR(s) = self {
            s
        } else {
            parens(self.expr()).as_lhs()
        }
    }

    fn first(&self) -> Token {
        match self {
            Self::SR(s) => s.first(),
            Self::EqOp(lhs, ..) => lhs.first(),
        }
    }
}
impl ConditionalExpr {
    fn sup(self) -> AssignExpr {
        AssignExpr::SC(self)
    }
    fn expr(self) -> Expr {
        self.sup().expr()
    }
    fn as_lor(self) -> LOrExpr {
        if let Self::SLO(s) = self {
            s
        } else {
            parens(self.expr()).as_lor()
        }
    }

    fn first(&self) -> Token {
        match self {
            Self::SLO(s) => s.first(),
            Self::Ternary(lhs, ..) => lhs.first(),
        }
    }
}
impl AssignExpr {
    fn sup(self) -> Expression {
        Expression::SA(self)
    }
    fn expr(self) -> Expr {
        self.sup().expr()
    }
    fn as_cond(self) -> ConditionalExpr {
        if let Self::SC(s) = self {
            s
        } else {
            parens(self.expr()).as_cond()
        }
    }

    fn first(&self) -> Token {
        match self {
            Self::SC(s) => s.first(),
            Self::Assignment(lhs, ..) => lhs.first(),
            Self::ArrowFunc(..) => Token::PAREN,
        }
    }
}
impl Expression {
    fn expr(self) -> Expr {
        Expr(self)
    }
    fn as_assign(self) -> AssignExpr {
        if let Self::SA(s) = self {
            s
        } else {
            parens(self.expr()).as_assign()
        }
    }
}

impl Expr {
    fn as_assign(self) -> AssignExpr {
        self.0.as_assign()
    }
    fn as_cond(self) -> ConditionalExpr {
        self.as_assign().as_cond()
    }
    fn as_lor(self) -> LOrExpr {
        self.as_cond().as_lor()
    }
    fn as_lhs(self) -> LHSExpr {
        self.as_lor().as_lhs()
    }
    fn as_member(self) -> MemberExpr {
        self.as_lhs().as_member()
    }
}

/////////////////////////////////////////////////////////////////////////////////////////////
fn parens(e: Expr) -> PrimaryExpr {
    PrimaryExpr::Paren(Box::new(e.0))
}

pub fn assign(lhs: Expr, rhs: Expr) -> Expr {
    AssignExpr::Assignment(lhs.as_lhs(), Box::new(rhs.as_assign())).expr()
}
pub fn call(lhs: Expr, rhs: Expr) -> Expr {
    CallExpr::Call(lhs.as_member(), Box::new(rhs.as_assign())).expr()
}
pub fn comma_pair(lhs: Expr, rhs: Expr) -> Expr {
    Expression::Comma(Box::new(lhs.0), rhs.as_assign()).expr()
}
pub fn eqop(lhs: Expr, rhs: Expr) -> Expr {
    EqualityExpr::EqOp(Box::new(lhs.as_lor()), rhs.as_lhs()).expr()
}
pub fn field(lhs: Expr, rhs: String) -> Expr {
    MemberExpr::Field(Box::new(lhs.as_member()), rhs).expr()
}
pub fn lit(v: Literal) -> Expr {
    PrimaryExpr::Literal(v).expr()
}
pub fn ternary(cond: Expr, e1: Expr, e2: Expr) -> Expr {
    ConditionalExpr::Ternary(cond.as_lor(), Box::new(e1.as_assign()), Box::new(e2.as_assign())).expr()
}
pub fn var(s: String) -> Expr {
    PrimaryExpr::Var(s).expr()
}

pub fn comma_list(mut exprs: Vec<Expr>) -> Expr {
    // Reverse the list so we can easily create a left-recursive structure instead of right recursive
    exprs.reverse();
    let mut res = exprs.pop().unwrap().0;
    while let Some(expr) = exprs.pop() {
        res = Expression::Comma(Box::new(res), expr.as_assign());
    }
    res.expr()
}

pub fn func(arg: String, scope: String, body: Expr) -> Expr {
    let mut body = body.as_assign();

    // body can't be an expression starting with "{"
    if body.first() == Token::BRACE {
        body = parens(body.expr()).as_assign();
    }

    AssignExpr::ArrowFunc(arg, scope, Box::new(body)).expr()
}

pub fn obj(fields: Vec<(String, Expr)>) -> Expr {
    let fields = fields.into_iter().map(|(k, v)| (k, Box::new(v.as_assign()))).collect();
    PrimaryExpr::Obj(fields).expr()
}
/////////////////////////////////////////////////////////////////////////////////////////////

impl PrimaryExpr {
    fn write(&self, out: &mut String) {
        match self {
            Self::Paren(e) => {
                *out += "(";
                e.write(out);
                *out += ")";
            }
            Self::Literal(v) => {
                use Literal::*;
                match v {
                    Bool(true) => *out += "true",
                    Bool(false) => *out += "false",
                    Str(v) => {
                        // TODO - escape strings
                        *out += "'";
                        *out += &v;
                        *out += "'";
                    }
                }
            }
            Self::Obj(fields) => {
                *out += "{";
                for (name, val) in fields {
                    *out += "'";
                    *out += name;
                    *out += "': ";
                    val.write(out);
                    *out += ", ";
                }
                *out += "}";
            }
            Self::Var(name) => {
                *out += name;
            }
        }
    }
}
impl MemberExpr {
    fn write(&self, out: &mut String) {
        match self {
            Self::SP(e) => e.write(out),
            Self::Field(lhs, rhs) => {
                lhs.write(out);
                *out += ".";
                *out += rhs;
            }
        }
    }
}
impl CallExpr {
    fn write(&self, out: &mut String) {
        match self {
            Self::SM(e) => e.write(out),
            Self::Call(lhs, rhs) => {
                lhs.write(out);
                *out += "(";
                rhs.write(out);
                *out += ")";
            }
        }
    }
}
impl EqualityExpr {
    fn write(&self, out: &mut String) {
        match self {
            Self::SR(e) => e.write(out),
            Self::EqOp(lhs, rhs) => {
                lhs.write(out);
                *out += " === ";
                rhs.write(out);
            }
        }
    }
}
impl ConditionalExpr {
    fn write(&self, out: &mut String) {
        match self {
            Self::SLO(e) => e.write(out),
            Self::Ternary(cond, e1, e2) => {
                cond.write(out);
                *out += " ? ";
                e1.write(out);
                *out += " : ";
                e2.write(out);
            }
        }
    }
}
impl AssignExpr {
    fn write(&self, out: &mut String) {
        match self {
            Self::SC(e) => e.write(out),
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
        }
    }
}
impl Expression {
    fn write(&self, out: &mut String) {
        match self {
            Self::SA(e) => e.write(out),
            Self::Comma(lhs, rhs) => {
                lhs.write(out);
                *out += ", ";
                rhs.write(out);
            }
        }
    }
}
impl Expr {
    pub fn to_source(&self) -> String {
        let mut s = "".to_string();
        self.0.write(&mut s);
        s
    }
}
