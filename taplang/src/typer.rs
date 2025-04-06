// use crate::ast::{ArithExpr, BoolExpr, Cmd, Expr, Lhs, Type, Typed};



// fn type_lhs(lhs: Lhs) -> Typed<Lhs> {
//     match lhs {
//         Lhs::Deref(lhs) => todo!(),
//         Lhs::Var(_) => todo!(),
//     }
// }

// fn type_bool(b: BoolExpr) -> Typed<BoolExpr> {
//     match b {
//         BoolExpr::Lvalue(lhs) => {
//             _ = type_lhs(*lhs);
//             todo!()
//         }
//         _ => (Type::Bool, b)
//     }
// }
// fn type_arith(a: ArithExpr) -> Typed<ArithExpr> {
//     match a {
//         ArithExpr::Lvalue(lhs) => {
//             _ = type_lhs(*lhs);
//             unimplemented!()
//         }
//         _ => (Type::Int, a)
//     }
// }
// fn type_expr(e: Expr) -> Typed<Expr> {
//     match e {
//         Expr::Lvalue(lhs) => todo!(),
//         Expr::Bool(bool_expr) => todo!(),
//         Expr::Int(arith_expr) => todo!(),
//         Expr::ImmutRef(_) => todo!(),
//         Expr::MutRef(_) => todo!(),
//         Expr::Tuple(exprs) => todo!(),
//     }
// }

// // For now, should be fine?
// // But the moment we have function dispatch that's
// //      dependent on the type, we need to have a proper Typed AST
// fn type_cmd(c: Cmd) -> Cmd {
//     match c {
//         Cmd::Skip => todo!(),
//         Cmd::Assign(lhs, expr) => todo!(),
//         Cmd::Sequence(cmd, cmd1) => todo!(),
//         Cmd::Let(_, _, expr) => todo!(),
//         Cmd::LetMut(_, _, expr) => todo!(),
//         Cmd::LetAlloc(_, _, arith_expr) => todo!(),
//         Cmd::LetMutAlloc(_, _, arith_expr) => todo!(),
//         Cmd::Free(lhs) => todo!(),
//         Cmd::While(bool_expr, cmd) => todo!(),
//         Cmd::If(bool_expr, cmd, cmd1) => todo!(),
//     }
// }