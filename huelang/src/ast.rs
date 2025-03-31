#[derive(Debug, Clone)]
pub enum Lhs {
    Deref(Box<Self>),
    Var(String),
}
#[derive(Debug, Clone)]
pub enum BoolExpr {
    True,
    False,
    Lvalue(Box<Lhs>),
    Bang(Box<Self>),
    And(Box<Self>, Box<Self>),
    Eq(Box<ArithExpr>, Box<ArithExpr>),
    Lt(Box<ArithExpr>, Box<ArithExpr>),
}
#[derive(Debug, Clone)]
pub enum ArithExpr {
    Nat(isize),
    Lvalue(Box<Lhs>),
    Neg(Box<Self>),
    Plus(Box<Self>, Box<Self>),
    Minus(Box<Self>, Box<Self>),
    Mult(Box<Self>, Box<Self>),
    Div(Box<Self>, Box<Self>),
    Sizeof(Box<Type>),
}
#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Bool,
    Ref {
        mutable: bool,
        inner_type: Box<Self>,
    },
    Loc(Box<Self>),
    Prod(Vec<Self>),
    // CustomType(String),
}
#[derive(Debug, Clone)]
pub enum Expr {
    Lvalue(Box<Lhs>),
    Bool(Box<BoolExpr>),
    Int(Box<ArithExpr>),
    ImmutRef(String),
    MutRef(String),
    Tuple(Vec<Self>),
}
#[derive(Debug, Clone)]
pub enum Cmd {
    Skip,
    Assign(Box<Lhs>, Box<Expr>),
    Sequence(Box<Self>, Box<Self>),
    Let(String, Box<Type>, Box<Expr>),
    LetMut(String, Box<Type>, Box<Expr>),
    LetAlloc(String, Box<Type>, Box<ArithExpr>),
    LetMutAlloc(String, Box<Type>, Box<ArithExpr>),
    Free(Box<Lhs>),
    While(Box<BoolExpr>, Box<Self>),
    If(Box<BoolExpr>, Box<Self>, Box<Self>),
}
