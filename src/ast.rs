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
pub enum LetPattern {
    Var(String),
    Record(Vec<(Spanned<String>, Box<LetPattern>)>),
}

#[derive(Debug, Clone)]
pub enum MatchPattern {
    Case(String, String),
    Wildcard(String),
}

#[derive(Debug, Clone)]
pub enum Expr {
    BinOp(Spanned<Box<Expr>>, Spanned<Box<Expr>>, OpType, Op, Span),
    Call(Box<Expr>, Box<Expr>, Span),
    Case(Spanned<String>, Box<Expr>),
    FieldAccess(Box<Expr>, String, Span),
    FuncDef(Spanned<(LetPattern, Box<Expr>)>),
    If(Spanned<Box<Expr>>, Box<Expr>, Box<Expr>),
    Let(VarDefinition, Box<Expr>),
    LetRec(Vec<VarDefinition>, Box<Expr>),
    Literal(Literal, Spanned<String>),
    Match(Box<Expr>, Vec<(Spanned<MatchPattern>, Box<Expr>)>, Span),
    NewRef(Box<Expr>, Span),
    Record(Option<Box<Expr>>, Vec<(Spanned<String>, Box<Expr>)>, Span),
    RefGet(Spanned<Box<Expr>>),
    RefSet(Spanned<Box<Expr>>, Box<Expr>),
    Typed(Box<Expr>, Box<TypeExpr>),
    Variable(Spanned<String>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Readability {
    ReadWrite,
    ReadOnly,
    WriteOnly,
}

#[derive(Debug, Clone)]
pub enum TypeExpr {
    Alias(Box<TypeExpr>, Spanned<String>),
    Case(Option<Box<TypeExpr>>, Vec<(Spanned<String>, Box<TypeExpr>)>, Span),
    Func(Spanned<(Box<TypeExpr>, Box<TypeExpr>)>),
    Ident(Spanned<String>),
    Nullable(Box<TypeExpr>, Span),
    Record(Option<Box<TypeExpr>>, Vec<(Spanned<String>, Box<TypeExpr>)>, Span),
    Ref(Box<TypeExpr>, Spanned<Readability>),
    TypeVar(Spanned<String>),
}

#[derive(Debug, Clone)]
pub enum TopLevel {
    Expr(Expr),
    LetDef(VarDefinition),
    LetRecDef(Vec<VarDefinition>),
}
