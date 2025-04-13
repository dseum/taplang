// pub type Typed<T> = (Type, T);

#[derive(Debug, Clone)]
pub enum Lhs {
    Var(String),
    Index(Box<Self>, isize),
    Deref(Box<Self>),
}
#[derive(Debug, Clone)]
pub enum Type {
    Unit,
    Int,
    Bool,
    RefMut(Box<Self>),
    Ref(Box<Self>),
    Loc(Box<Self>),
    Prod(Vec<Self>),
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
pub enum Expr {
    Index(Box<Self>, isize),
    Lvalue(Box<Lhs>),
    ImmutRef(String),
    MutRef(String),
    Tuple(Vec<Self>),
    Unit,
    Alloc(Box<Self>),
    Free(Box<Lhs>),
    Call(String, Vec<Self>),
    BoundCall(String, String, Vec<Self>),
    // booleans
    True,
    False,
    Bang(Box<Self>),
    And(Box<Self>, Box<Self>),
    Or(Box<Self>, Box<Self>),
    Eq(Box<Self>, Box<Self>),
    Lt(Box<Self>, Box<Self>),
    // arithemtic
    Nat(isize),
    Neg(Box<Self>),
    Plus(Box<Self>, Box<Self>),
    Minus(Box<Self>, Box<Self>),
    Mult(Box<Self>, Box<Self>),
    Div(Box<Self>, Box<Self>),
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
    Not(Box<Self>),
    Or(Box<Self>, Box<Self>),
    And(Box<Self>, Box<Self>),
    Implies(Box<Self>, Box<Self>),
    Forall(String, Box<Self>),
    Exists(String, Box<Self>),
    Emp,
    Pointsto(String, Box<Value>),
    SepConj(Box<Self>, Box<Self>),
    MagicWand(Box<Self>, Box<Self>),
}
#[derive(Debug, Clone)]
pub enum FnDef {
    FnDef(String, Vec<(String, Type)>, Box<Type>, Box<Cmd>, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum ImplFnDef {
    ImplFnDef(
        String,
        Vec<(String, Type)>,
        Box<Type>,
        Box<HeapPre>,
        Box<HeapPost>,
        Box<KCmd>,
        Box<Expr>,
    ),
}

#[derive(Debug, Clone)]
pub enum Decl {
    TypeDef(String, Box<Type>),
    TypeImpl(String, HeapPred, Vec<ImplFnDef>),
}

#[derive(Debug, Clone)]
pub enum Cmd {
    TypeDecl(Box<Decl>),
    FxnDefin(Box<FnDef>),
    Skip,
    Scope(Box<Self>),
    Assign(Box<Lhs>, Box<Expr>),
    Sequence(Box<Self>, Box<Self>),
    Let(String, Box<Type>, Box<Expr>),
    LetMut(String, Box<Type>, Box<Expr>),
    While(Box<Expr>, Box<Self>),
    If(Box<Expr>, Box<Self>, Box<Self>),
}

#[derive(Debug, Clone)]
pub enum KCmd {
    Command(Box<Cmd>),
    Sequence(Box<Self>, Box<Self>),
    Lemma(Box<HeapPred>),
}
