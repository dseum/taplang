use std::ops::Range;

pub type Span = Range<usize>;
pub type Spanned<T> = (T, Span);

#[derive(Debug, Clone)]
pub enum Lhs<T> {
    Var(String, T),
    Index(Box<Spanned<Self>>, isize, T),
    DerefRef(Box<Spanned<Self>>, T),
    DerefPointer(Box<Spanned<Self>>, T),
}
impl<T> Lhs<T> {
    pub fn get_type(&self) -> &T {
        use Lhs::*;
        match self {
            Var(_, t) => t,
            Index(_, _, t) => t,
            DerefRef(_, t) => t,
            DerefPointer(_, t) => t,
        }
    }
}
#[derive(Debug, Clone)]
pub enum Type {
    Unit,
    Int,
    Bool,
    RefMut(Box<Spanned<Self>>),
    Ref(Box<Spanned<Self>>),
    Loc(Box<Spanned<Self>>),
    Prod(Vec<Spanned<Self>>),
    CustomType(String),
}
#[derive(Debug, Clone)]
pub enum HeapPre {
    Vacuous,
    Pre(Box<HeapPred>),
}
#[derive(Debug, Clone)]
pub enum HeapPost {
    Vacuous,
    Post(Box<HeapPred>),
}
#[derive(Debug, Clone)]
pub enum Expr<T> {
    Index(Box<Spanned<Self>>, isize, T),
    Lvalue(Box<Lhs<T>>, T),
    ImmutRef(String, T),
    MutRef(String, T),
    Tuple(Vec<Spanned<Self>>, T),
    Unit(T),
    Alloc(Box<Spanned<Self>>, T),
    Free(Box<Spanned<Lhs<T>>>, T),
    Call(String, Vec<Spanned<Self>>, T),
    BoundCall(String, String, Vec<Spanned<Self>>, T),
    // booleans
    True(T),
    False(T),
    Bang(Box<Spanned<Self>>, T),
    And(Box<Spanned<Self>>, Box<Spanned<Self>>, T),
    Or(Box<Spanned<Self>>, Box<Spanned<Self>>, T),
    Eq(Box<Spanned<Self>>, Box<Spanned<Self>>, T),
    Lt(Box<Spanned<Self>>, Box<Spanned<Self>>, T),
    // arithemtic
    Nat(isize, T),
    Neg(Box<Spanned<Self>>, T),
    Plus(Box<Spanned<Self>>, Box<Spanned<Self>>, T),
    Minus(Box<Spanned<Self>>, Box<Spanned<Self>>, T),
    Mult(Box<Spanned<Self>>, Box<Spanned<Self>>, T),
    Div(Box<Spanned<Self>>, Box<Spanned<Self>>, T),
}
impl<T> Expr<T> {
    pub fn get_type(&self) -> &T {
        use Expr::*;
        match self {
            Index(_, _, t) => t,
            Lvalue(_, t) => t,
            ImmutRef(_, t) => t,
            MutRef(_, t) => t,
            Tuple(_, t) => t,
            Unit(t) => t,
            Alloc(_, t) => t,
            Free(_, t) => t,
            Call(_, _, t) => t,
            BoundCall(_, _, _, t) => t,
            True(t) => t,
            False(t) => t,
            Bang(_, t) => t,
            And(_, _, t) => t,
            Or(_, _, t) => t,
            Eq(_, _, t) => t,
            Lt(_, _, t) => t,
            Nat(_, t) => t,
            Neg(_, t) => t,
            Plus(_, _, t) => t,
            Minus(_, _, t) => t,
            Mult(_, _, t) => t,
            Div(_, _, t) => t,
        }
    }
}
#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    Int(isize),
    True,
    False,
}
#[derive(Debug, Clone)]
pub enum HeapPred {
    Var(String),
    Not(Box<Spanned<Self>>),
    Or(Box<Spanned<Self>>, Box<Spanned<Self>>),
    And(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Implies(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Forall(String, Box<Spanned<Self>>),
    Exists(String, Box<Spanned<Self>>),
    Emp,
    Pointsto(String, Box<Value>),
    SepConj(Box<Spanned<Self>>, Box<Spanned<Self>>),
    MagicWand(Box<Spanned<Self>>, Box<Spanned<Self>>),
}

#[derive(Debug, Clone)]
pub struct FnDef<T>(
    pub Spanned<String>,
    pub Vec<Spanned<(String, Type)>>,
    pub Box<Spanned<Type>>,
    pub Box<Spanned<Cmd<T>>>,
    pub Box<Spanned<Expr<T>>>,
);

#[derive(Debug, Clone)]
pub struct ImplFnDef<T>(
    pub String,
    pub Vec<(String, Type)>,
    pub Box<Type>,
    pub Box<HeapPre>,
    pub Box<HeapPost>,
    pub Box<Cmd<T>>,
    pub Box<Expr<T>>,
);

#[derive(Debug, Clone)]
pub enum Decl<T> {
    TypeDef(String, Vec<Type>),
    TypeImpl(String, HeapPred, Vec<ImplFnDef<T>>),
}

#[derive(Debug, Clone)]
pub enum Cmd<T> {
    TypeDecl(Box<Decl<T>>),
    FxnDefin(Box<FnDef<T>>),
    Skip,
    Scope(Box<Spanned<Self>>),
    Assign(Box<Lhs<T>>, Box<Expr<T>>),
    Sequence(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Let(String, Box<Type>, Box<Expr<T>>),
    LetMut(String, Box<Type>, Box<Expr<T>>),
    While(Box<Expr<T>>, Box<Spanned<Self>>),
    If(Box<Expr<T>>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Lemma(Box<HeapPred>),
}
