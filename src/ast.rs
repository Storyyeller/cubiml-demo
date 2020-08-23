use crate::spans::{Span, Spanned};

#[derive(Debug, Clone)]
pub enum Literal {
    Bool,
    Float,
    Int,
    Null,
    Str,
}

#[derive(Debug, Clone)]
pub enum Op {
    Add,
    Sub,
    Mult,
    Div,
    Rem,

    Lt,
    Lte,
    Gt,
    Gte,

    Eq,
    Neq,
}

#[derive(Debug, Clone)]
pub enum OpType {
    IntOp,
    FloatOp,
    StrOp,

    IntOrFloatCmp,
    AnyCmp,
}

type VarDefinition = (String, Box<Expr>);

#[derive(Debug, Clone)]
pub enum Pattern {
    Case(String, String),
    Wildcard(String),
}

#[derive(Debug, Clone)]
pub enum Expr {
    BinOp(Spanned<Box<Expr>>, Spanned<Box<Expr>>, OpType, Op, Span),
    Call(Box<Expr>, Box<Expr>, Span),
    Case(Spanned<String>, Box<Expr>),
    FieldAccess(Box<Expr>, String, Span),
    FuncDef(Spanned<(String, Box<Expr>)>),
    If(Spanned<Box<Expr>>, Box<Expr>, Box<Expr>),
    Let(VarDefinition, Box<Expr>),
    LetRec(Vec<VarDefinition>, Box<Expr>),
    Literal(Literal, Spanned<String>),
    Match(Box<Expr>, Vec<(Spanned<Pattern>, Box<Expr>)>, Span),
    NewRef(Box<Expr>, Span),
    Record(Option<Box<Expr>>, Vec<(Spanned<String>, Box<Expr>)>, Span),
    RefGet(Spanned<Box<Expr>>),
    RefSet(Spanned<Box<Expr>>, Box<Expr>),
    Variable(Spanned<String>),
}

#[derive(Debug, Clone)]
pub enum TopLevel {
    Expr(Expr),
    LetDef(VarDefinition),
    LetRecDef(Vec<VarDefinition>),
}
