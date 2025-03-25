use chumsky::span::SimpleSpan;

#[derive(Debug, PartialEq)]
pub struct Spanned<T>(pub T, pub SimpleSpan<usize>);

#[derive(Debug, Clone)]
pub enum Lhs<'src> {
    Deref(Box<Self>),
    Var(&'src str),
}
#[derive(Debug, Clone)]
pub enum BoolExpr<'src> {
    True,
    False,
    Lvalue(Box<Lhs<'src>>),
    Bang(Box<Self>),
    And(Box<Self>, Box<Self>),
    Eq(Box<ArithExpr<'src>>, Box<ArithExpr<'src>>),
    Lt(Box<ArithExpr<'src>>, Box<ArithExpr<'src>>),
}
#[derive(Debug, Clone)]
pub enum ArithExpr<'src> {
    Int(i64),
    Lvalue(Box<Lhs<'src>>),
    Neg(Box<Self>),
    Plus(Box<Self>, Box<Self>),
    Minus(Box<Self>, Box<Self>),
    Mult(Box<Self>, Box<Self>),
    Div(Box<Self>, Box<Self>),
    Sizeof(Box<Type<'src>>),
}
#[derive(Debug, Clone)]
pub enum Type<'src> {
    Int,
    Bool,
    Ref {
        mutable: bool,
        inner_type: Box<Type<'src>>,
    },
    CustomType(&'src str),
}
#[derive(Debug, Clone)]
pub enum Expr<'src> {
    Lvalue(Box<Lhs<'src>>),
    Bool(Box<BoolExpr<'src>>),
    Int(Box<ArithExpr<'src>>),
    ImmutRef(&'src str),
    MutRef(&'src str),
    Tuple(Vec<Self>),
}
#[derive(Debug, Clone)]
pub enum Cmd<'src> {
    Skip,
    Assign(Box<Lhs<'src>>, Box<Expr<'src>>),
    Sequence(Box<Self>, Box<Self>),
    Print(Box<Expr<'src>>),
    Let(&'src str, Box<Expr<'src>>),
    LetMut(&'src str, Box<Expr<'src>>),
    LetAlloc(&'src str, Box<ArithExpr<'src>>),
    LetMutAlloc(&'src str, Box<ArithExpr<'src>>),
    Free(Box<Lhs<'src>>),
    While(Box<BoolExpr<'src>>, Box<Self>),
    If(Box<BoolExpr<'src>>, Box<Self>, Box<Self>),
}
