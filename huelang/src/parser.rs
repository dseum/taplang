// Values v ∈ V ::= n | true | false
// L-Values lhs ∈ L ::= ∗lhs | x
// BoolExpr b ∈ B ::= true | false | (b) | lhs | ¬b | b1 && b2 | a1 = a2 | a1 < a2
// ArithExpr a ∈ A ::= n | (a) | lhs | −a | a1 + a2 | a1 − a2 | a1 ∗ a2 | a1/a2 | sizeof(τ )
// Expr e ∈ E ::= a | b | &x | &mut x | [ei]l
// i=1
// Command c ∈ C ::= skip | lhs = e | c1; c2 | print e | let x = e
// | let mut x = e | let x = alloc(a) | free(lhs)
// | let mut x = alloc(a) | while b do c
// | if b then c1 else c2
// HeapPreds p ∈ P, Q, R ::= b | ¬P | P ∨ Q | P ∧ Q | P =⇒ Q | ∀x.P | ∃x.P
// | emp | E1 7 → E2 | P ∗ ∗Q | P − ∗Q
use chumsky::prelude::*;
// use crate::lexer::Token;
use crate::ast::*;

#[allow(clippy::let_and_return)]
fn lhs_parser<'src>() -> impl Parser<'src, &'src str, Lhs<'src>> + Clone {
    let ident = text::ascii::ident().padded().map(Lhs::Var);

    let lvalue = just('*')
        .padded()
        .repeated()
        .foldr(ident, |_op, lhs| Lhs::Deref(Box::new(lhs)));
    lvalue
}

#[allow(clippy::let_and_return)]
fn type_parser<'src>() -> impl Parser<'src, &'src str, Type<'src>> + Clone {
    recursive(|type_shi| {
        let atom = choice((
            just("int").to(Type::Int),
            just("bool").to(Type::Bool),
            just("ref").then((just("true").to(true)).or(just("false").to(false))).then(type_shi).map(|((_, b), t)| Type::Ref { mutable: b, inner_type: Box::new(t) })
        ));
        atom
    })
}

#[allow(clippy::let_and_return)]
fn arith_parser<'src>() -> impl Parser<'src, &'src str, ArithExpr<'src>> + Clone {
    let op = |c: char| just(c).padded();
    let sop = |c: &'src str| just(c).padded();
    recursive(|arith_expr| {
        let atom = text::int(10).map(|s: &str| ArithExpr::Int(s.parse().unwrap()))
        .or(lhs_parser().map(|lhs| ArithExpr::Lvalue(Box::new(lhs))))
        .or(arith_expr.delimited_by(just('('), just(')')))
        .or(sop("sizeof").ignore_then(type_parser()).map(|t| ArithExpr::Sizeof(Box::new(t))))
        ;
        let unary = op('-')
            .repeated()
            .foldr(atom, |_op, rhs| ArithExpr::Neg(Box::new(rhs)));

        let product = unary.clone().foldl(
            choice((
                op('*').to(ArithExpr::Mult as fn(_, _) -> _),
                op('/').to(ArithExpr::Div as fn(_, _) -> _),
            ))
            .then(unary)
            .repeated(),
            |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
        );

        let sum = product.clone().foldl(
            choice((
                op('+').to(ArithExpr::Plus as fn(_, _) -> _),
                op('-').to(ArithExpr::Minus as fn(_, _) -> _),
            ))
            .then(product)
            .repeated(),
            |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
        );
        sum
    })
}

#[allow(clippy::let_and_return)]
pub fn bool_parser<'src>() -> impl Parser<'src, &'src str, BoolExpr<'src>> + Clone {
    let op = |c: char| just(c).padded();
    let sop = |c: &'src str| just(c).padded();
    recursive(|bool_expr| {
        let atom = choice((
            just("true").to(BoolExpr::True),
            just("false").to(BoolExpr::False),
        ))
        .or(lhs_parser().map(|lhs| BoolExpr::Lvalue(Box::new(lhs))))
        .or(bool_expr.delimited_by(just('('), just(')')))
        .or(arith_parser().then_ignore(sop("==")).then(arith_parser()).map(|(a1, a2)| BoolExpr::Eq(Box::new(a1), Box::new(a2))))
        .or(arith_parser().then_ignore(op('<')).then(arith_parser()).map(|(a1, a2)| BoolExpr::Lt(Box::new(a1), Box::new(a2))));

        let unary = op('!')
            .repeated()
            .foldr(atom, |_op, rhs| BoolExpr::Bang(Box::new(rhs)));

        let binary = unary.clone().foldl(
            choice((
                sop("&&").to(BoolExpr::And as fn(_, _) -> _),
                // sop('').to(BoolExpr::Div as fn(_, _) -> _),
            ))
            .then(unary)
            .repeated(),
            |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
        );
        binary
    })
}

#[allow(clippy::let_and_return)]
pub fn expr_parser<'src>() -> impl Parser<'src, &'src str, Expr<'src>> + Clone {
    let ident = text::ascii::ident().padded();
    let op = |c: char| just(c).padded();
    let sop = |c: &'src str| just(c).padded();
    recursive(|expr| {
        choice((
            expr.clone().delimited_by(just('('), just(')')),
            expr.repeated().collect().map(|v: Vec<_>| Expr::Tuple(v)).delimited_by(just('['), just(']')),
            op('&').ignore_then(ident.map(Expr::ImmutRef)),
            sop("&mut").ignore_then(ident.clone().map(Expr::MutRef)),
            bool_parser().map(|b| Expr::Bool(Box::new(b))),
            arith_parser().map(|a| Expr::Int(Box::new(a))),
    ))
    })
}

#[allow(clippy::let_and_return)]
pub fn command_parser<'src>() -> impl Parser<'src, &'src str, Cmd<'src>> + Clone {
    let ident = text::ascii::ident().padded();
    let op = |c: char| just(c).padded();
    let sop = |c: &'src str| just(c).padded();
    let cmd = recursive(|cmd| {
        choice((
            sop("skip").to(Cmd::Skip),
            sop("print").ignore_then(expr_parser()).map(|e| Cmd::Print(Box::new(e))),
            sop("let").ignore_then(ident).then_ignore(op('=')).then(expr_parser()).map(|(v, e)| Cmd::Let(v, Box::new(e))),
            sop("let").ignore_then(sop("mut")).ignore_then(ident).then_ignore(op('=')).then(expr_parser()).map(|(v, e)| Cmd::LetMut(v, Box::new(e))),
            sop("let").ignore_then(ident).then_ignore(op('=').then(sop("alloc")).then(op('('))).then(arith_parser()).then_ignore(op(')')).map(|(v, a)| Cmd::LetAlloc(v, Box::new(a))),
            sop("let").ignore_then(sop("mut")).ignore_then(ident).then_ignore(op('=').then(sop("alloc")).then(op('('))).then(arith_parser()).then_ignore(op(')')).map(|(v, a)| Cmd::LetMutAlloc(v, Box::new(a))),
            lhs_parser().then_ignore(op('=')).then(expr_parser()).map(|(lhs, e)| Cmd::Assign(Box::new(lhs), Box::new(e))),
            sop("free").ignore_then(op('(')).ignore_then(lhs_parser()).then_ignore(op(')')).map(|lhs| Cmd::Free(Box::new(lhs))),
            sop("while").ignore_then(bool_parser()).then_ignore(sop("do")).then(cmd.clone()).map(|(b, c)| Cmd::While(Box::new(b), Box::new(c))),
            sop("if").ignore_then(bool_parser()).then_ignore(sop("then")).then(cmd.clone()).then(sop("else").ignore_then(cmd.clone())).map(|((b, c1), c2)| Cmd::If(Box::new(b), Box::new(c1), Box::new(c2))),
            // cmd.clone().then_ignore(op(';')).then(cmd).map(|(c1, c2)| Cmd::Sequence(Box::new(c1), Box::new(c2)))
    ))
    });

    cmd.clone().foldl(
        choice((
            op(';').to(Cmd::Sequence as fn(_, _) -> _),
        ))
        .then(cmd)
        .repeated(),
        |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
    )
}
