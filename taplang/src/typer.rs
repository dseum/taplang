// use std::collections::HashMap;

// use crate::ast::{self, Cmd, Decl, Expr, FnDef, ImplFnDef, Lhs};

// #[derive(Debug, Clone, PartialEq)]
// pub enum DataType {
//     Unit,
//     Int,
//     Bool,
//     RefMut(Box<Self>),
//     Ref(Box<Self>),
//     Loc(Box<Self>),
//     Prod(Vec<Self>),
//     CustomType(String),
//     Arrow(Vec<Self>, Box<Self>),
// }

// impl DataType {
//     pub fn from_type(tau: ast::Type) -> Self {
//         match tau {
//             ast::Type::Unit => Self::Unit,
//             ast::Type::Int => Self::Int,
//             ast::Type::Bool => Self::Bool,
//             ast::Type::RefMut(x) => Self::RefMut(Box::new(Self::from_type(*x))),
//             ast::Type::Ref(x) => Self::Ref(Box::new(Self::from_type(*x))),
//             ast::Type::Loc(x) => Self::Loc(Box::new(Self::from_type(*x))),
//             ast::Type::Prod(xs) => Self::Prod(xs.into_iter().map(Self::from_type).collect()),
//             ast::Type::CustomType(name) => Self::CustomType(name),
//         }
//     }
// }

// macro_rules! typed {
//     ($t:ident) => {
//         $t<DataType>
//     };
// }

// // have internal arrow/exp type.

// // Let ϕ be the stack of functions, γ the stack of implementations, η the map of the machine
// // heap, and σ the stack of the machine stack.

// // fn type_lhs(lhs: Lhs) -> Typed<Lhs> {
// //     match lhs {
// //         Lhs::Deref(lhs) => todo!(),
// //         Lhs::Var(_) => todo!(),
// //         Lhs::Index(inner_lhs, i) => todo!(),
// //     }
// // }

// pub struct Liveness {}
// pub struct Gamma {
//     type_context: Vec<HashMap<String, (DataType, bool)>>,
// }
// impl Gamma {
//     pub fn new() -> Self {
//         Self {
//             type_context: vec![HashMap::new()],
//         }
//     }
//     pub fn push_level(&mut self) {
//         self.type_context.push(HashMap::new());
//     }
//     pub fn pop_level(&mut self) {
//         self.type_context.pop();
//     }
//     pub fn declare_new(&mut self, name: String, tau: DataType, is_mut: bool) {
//         if let Some(level) = self.type_context.iter_mut().last() {
//             level.insert(name, (tau, is_mut));
//         } else {
//             panic!("Empty gamma????")
//         }
//     }
//     pub fn get(&self, name: &String) -> Option<&(DataType, bool)> {
//         self.type_context
//             .iter()
//             .rev()
//             .find_map(|level| level.get(name))
//     }
// }

// fn assert_type_eq(t1: DataType, t2: DataType) {
//     assert!(t1 == t2)
// }

// fn type_expr(e: Expr<()>, gamma: &Gamma) -> typed!(Expr) {
//     use Expr::*;
//     match e {
//         Index(e1, i, _) => todo!(),
//         Lvalue(lhs, T) => todo!(),
//         Call(fn_name, exprs, T) => todo!(),
//         BoundCall(ty_name, fn_name, exprs, T) => todo!(),
//         ImmutRef(name, T) => todo!(),
//         MutRef(name, T) => todo!(),
//         Free(lhs, _) => Free(lhs, DataType::Unit),
//         Alloc(e, _) => {
//             let te = type_expr(*e, gamma);
//             let ty = te.get_type().to_owned();
//             Alloc(Box::new(te), DataType::Loc(Box::new(ty)))
//         }
//         Tuple(exprs, _) => {
//             let texprs = exprs
//                 .into_iter()
//                 .map(|e| type_expr(e, gamma))
//                 .collect::<Vec<_>>();
//             let ty = DataType::Prod(
//                 texprs
//                     .iter()
//                     .map(|te| te.get_type().to_owned())
//                     .collect::<Vec<_>>(),
//             );
//             Tuple(texprs, ty)
//         }
//         Unit(_) => Unit(DataType::Unit),
//         // booleans
//         True(_) => True(DataType::Bool),
//         False(_) => False(DataType::Bool),
//         Bang(e, _) => {
//             let te = type_expr(*e, gamma);
//             assert_type_eq(te.get_type().to_owned(), DataType::Bool);
//             Bang(Box::new(te), DataType::Bool)
//         }
//         And(e1, e2, _) => {
//             let te1 = type_expr(*e1, gamma);
//             assert_type_eq(te1.get_type().to_owned(), DataType::Bool);
//             let te2 = type_expr(*e2, gamma);
//             assert_type_eq(te2.get_type().to_owned(), DataType::Bool);
//             And(Box::new(te1), Box::new(te2), DataType::Bool)
//         }
//         Or(e1, e2, T) => {
//             let te1 = type_expr(*e1, gamma);
//             assert_type_eq(te1.get_type().to_owned(), DataType::Bool);
//             let te2 = type_expr(*e2, gamma);
//             assert_type_eq(te2.get_type().to_owned(), DataType::Bool);
//             Or(Box::new(te1), Box::new(te2), DataType::Bool)
//         }
//         Eq(e1, e2, _) => {
//             let te1 = type_expr(*e1, gamma);
//             let te2 = type_expr(*e2, gamma);
//             assert_type_eq(te1.get_type().to_owned(), te2.get_type().to_owned());
//             Eq(Box::new(te1), Box::new(te2), DataType::Bool)
//         }
//         Lt(e1, e2, _) => {
//             let te1 = type_expr(*e1, gamma);
//             let te2 = type_expr(*e2, gamma);
//             assert_type_eq(te1.get_type().to_owned(), te2.get_type().to_owned());
//             Lt(Box::new(te1), Box::new(te2), DataType::Bool)
//         }
//         // arithemtic
//         Nat(i, _) => Nat(i, DataType::Int),
//         Neg(e, _) => {
//             let te = type_expr(*e, gamma);
//             assert_type_eq(te.get_type().to_owned(), DataType::Int);
//             Neg(Box::new(te), DataType::Int)
//         }
//         Plus(e1, e2, _) => {
//             let te1 = type_expr(*e1, gamma);
//             assert_type_eq(te1.get_type().to_owned(), DataType::Int);
//             let te2 = type_expr(*e2, gamma);
//             assert_type_eq(te2.get_type().to_owned(), DataType::Int);
//             Plus(Box::new(te1), Box::new(te2), DataType::Int)
//         }
//         Minus(e1, e2, _) => {
//             let te1 = type_expr(*e1, gamma);
//             assert_type_eq(te1.get_type().to_owned(), DataType::Int);
//             let te2 = type_expr(*e2, gamma);
//             assert_type_eq(te2.get_type().to_owned(), DataType::Int);
//             Minus(Box::new(te1), Box::new(te2), DataType::Int)
//         }
//         Mult(e1, e2, _) => {
//             let te1 = type_expr(*e1, gamma);
//             assert_type_eq(te1.get_type().to_owned(), DataType::Int);
//             let te2 = type_expr(*e2, gamma);
//             assert_type_eq(te2.get_type().to_owned(), DataType::Int);
//             Mult(Box::new(te1), Box::new(te2), DataType::Int)
//         }
//         Div(e1, e2, _) => {
//             let te1 = type_expr(*e1, gamma);
//             assert_type_eq(te1.get_type().to_owned(), DataType::Int);
//             let te2 = type_expr(*e2, gamma);
//             assert_type_eq(te2.get_type().to_owned(), DataType::Int);
//             Div(Box::new(te1), Box::new(te2), DataType::Int)
//         }
//     }
// }

// fn type_fxn(d: FnDef<()>) -> typed!(FnDef) {
//     let FnDef(name, args, ret, body, output) = d;
//     FnDef(
//         name,
//         args,
//         ret,
//         Box::new(type_cmd(*body)),
//         Box::new(type_expr(*output)),
//     )
// }

// fn type_fxn_impl(d: ImplFnDef<()>, gamma: &mut Gamma) -> typed!(ImplFnDef) {
//     let ImplFnDef(name, args, ret, hpre, hpost, body, output) = d;

//     let body = Box::new(type_cmd(*body, gamma));
//     let output = Box::new(type_expr(*output, &gamma));
//     ImplFnDef(name, args, ret, hpre, hpost, body, output)
// }

// fn type_decl(d: Decl<()>) -> typed!(Decl) {
//     use Decl::*;
//     match d {
//         TypeDef(name, types) => TypeDef(name, types),
//         TypeImpl(name, hp, fxn_impls) => TypeImpl(
//             name,
//             hp,
//             fxn_impls.into_iter().map(type_fxn_impl).collect::<Vec<_>>(),
//         ),
//     }
// }

// pub fn type_cmd(c: Cmd<()>, gamma: &mut Gamma) -> typed!(Cmd) {
//     use Cmd::*;
//     match c {
//         Skip => Skip,
//         Assign(lhs, expr) => {
//             type_lhs(lhs);


//             Assign(lhs, Box::new(type_expr(*expr, gamma)))
//         },
//         Sequence(cmd1, cmd2) => Sequence(
//             Box::new(type_cmd(*cmd1, gamma)),
//             Box::new(type_cmd(*cmd2, gamma)),
//         ),
//         Let(name, tau, expr) => {
//             gamma.declare_new(name.clone(), DataType::from_type(*tau.clone()), false);
//             let letcmd = Let(name, tau, Box::new(type_expr(*expr, gamma)));
//             letcmd
//         }
//         LetMut(name, tau, expr) => {
//             gamma.declare_new(name.clone(), DataType::from_type(*tau.clone()), true);
//             let letcmd = Let(name, tau, Box::new(type_expr(*expr, gamma)));
//             letcmd
//         }
//         While(e, cmd) => While(
//             Box::new(type_expr(*e, gamma)),
//             Box::new(type_cmd(*cmd, gamma)),
//         ),
//         If(e, cmd1, cmd2) => If(
//             Box::new(type_expr(*e, gamma)),
//             Box::new(type_cmd(*cmd1, gamma)),
//             Box::new(type_cmd(*cmd2, gamma)),
//         ),
//         Lemma(h) => Lemma(h),
//         TypeDecl(d) => TypeDecl(Box::new(type_decl(*d))),
//         FxnDefin(fxn) => FxnDefin(Box::new(type_fxn(*fxn))),
//         Scope(cmd) => {
//             gamma.push_level();
//             let scope = Scope(Box::new(type_cmd(*cmd, gamma)));
//             gamma.pop_level();
//             scope
//         }
//     }
// }
