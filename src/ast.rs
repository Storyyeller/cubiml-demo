#[derive(Debug)]
pub enum Literal {
    Bool,
    Float,
    Int,
    Str,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum OpType {
    IntOp,
    FloatOp,
    StrOp,

    IntCmp,
    FloatCmp,
    AnyCmp,
}

type VarDefinition = (String, Box<Expr>);
type CaseMatchPattern = (String, String);

#[derive(Debug)]
pub enum Expr {
    BinOp(Box<Expr>, Box<Expr>, OpType, Op),
    Call(Box<Expr>, Box<Expr>),
    Case(String, Box<Expr>),
    FieldAccess(Box<Expr>, String),
    FuncDef(String, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Let(VarDefinition, Box<Expr>),
    LetRec(Vec<VarDefinition>, Box<Expr>),
    Literal(Literal, String),
    Match(Box<Expr>, Vec<(CaseMatchPattern, Box<Expr>)>),
    Record(Vec<(String, Box<Expr>)>),
    Variable(String),
}

#[derive(Debug)]
pub enum TopLevel {
    Expr(Expr),
    LetDef(VarDefinition),
    LetRecDef(Vec<VarDefinition>),
}
