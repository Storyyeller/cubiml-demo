#[derive(Debug)]
pub enum Literal {
    Bool(bool),
}

type VarDefinition = (String, Box<Expr>);
type CaseMatchPattern = (String, String);

#[derive(Debug)]
pub enum Expr {
    Call(Box<Expr>, Box<Expr>),
    Case(String, Box<Expr>),
    FieldAccess(Box<Expr>, String),
    FuncDef(String, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Let(VarDefinition, Box<Expr>),
    LetRec(Vec<VarDefinition>, Box<Expr>),
    Literal(Literal),
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
