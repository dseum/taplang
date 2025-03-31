use crate::ast::{ArithExpr, BoolExpr, Cmd, Expr, Lhs, Type};
use crate::lexer::Token;
use chumsky::{input::ValueInput, prelude::*};

pub fn lhs_parser<'src, I>() -> impl Parser<'src, I, Lhs, extra::Err<Rich<'src, Token>>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    let ident = select! {
        Token::Var(s) => Lhs::Var(s),
    };
    just(Token::Multiply)
        .repeated()
        .foldr(ident, |_star, inner| Lhs::Deref(Box::new(inner)))
}

pub fn type_parser<'src, I>() -> impl Parser<'src, I, Type, extra::Err<Rich<'src, Token>>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    let whitespace = just(Token::Whitespace).repeated().ignored();

    recursive(|typeshi| {
        let atom = select! {
            Token::Int => Type::Int,
            Token::Bool => Type::Bool
        };
        choice((
            atom,
            just(Token::Ref)
                .ignore_then(just(Token::LessThan).padded_by(whitespace.clone()))
                .ignore_then(just(Token::True).to(true).or(just(Token::False).to(false)))
                .then_ignore(just(Token::Comma))
                .then(typeshi.clone())
                .then_ignore(just(Token::GreaterThan).padded_by(whitespace.clone()))
                .map(|(b, tau)| Type::Ref {
                    mutable: b,
                    inner_type: Box::new(tau),
                }),
            just(Token::Loc)
                .ignore_then(just(Token::LessThan).padded_by(whitespace.clone()))
                .ignore_then(typeshi.clone())
                .then_ignore(just(Token::GreaterThan).padded_by(whitespace.clone()))
                .map(|tau| Type::Loc(Box::new(tau))),
            typeshi
                .clone()
                .separated_by(just(Token::Comma).padded_by(whitespace.clone()))
                .allow_trailing()
                .collect()
                .delimited_by(
                    just(Token::LSqBra).padded_by(whitespace.clone()),
                    just(Token::RSqBra).padded_by(whitespace.clone()),
                )
                .map(|v| Type::Prod(v)),
        ))
        .padded_by(whitespace)
    })
}

pub fn arith_parser<'src, I>()
-> impl Parser<'src, I, ArithExpr, extra::Err<Rich<'src, Token>>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    let whitespace = just(Token::Whitespace).repeated().ignored();
    let op = |tok| just(tok).padded_by(whitespace.clone());

    recursive(|arith_expr| {
        let literal = select! {
            Token::Nat(n) => ArithExpr::Nat(n),
        };

        let atom = choice((
            literal,
            lhs_parser().map(|lhs| ArithExpr::Lvalue(Box::new(lhs))),
            arith_expr.delimited_by(op(Token::LParen), op(Token::RParen)),
            op(Token::Sizeof)
                .ignore_then(type_parser())
                .map(|t| ArithExpr::Sizeof(Box::new(t))),
        ));

        let unary = op(Token::Minus)
            .repeated()
            .foldr(atom, |_op, rhs| ArithExpr::Neg(Box::new(rhs)));

        let product = unary.clone().foldl(
            choice((
                op(Token::Multiply).to(ArithExpr::Mult as fn(_, _) -> _),
                op(Token::Divide).to(ArithExpr::Div as fn(_, _) -> _),
            ))
            .then(unary)
            .repeated(),
            |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
        );
        let sum = product.clone().foldl(
            choice((
                op(Token::Plus).to(ArithExpr::Plus as fn(_, _) -> _),
                op(Token::Minus).to(ArithExpr::Minus as fn(_, _) -> _),
            ))
            .then(product)
            .repeated(),
            |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
        );
        sum
    })
    .padded_by(whitespace)
}

pub fn bool_parser<'src, I>()
-> impl Parser<'src, I, BoolExpr, extra::Err<Rich<'src, Token>>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    let whitespace = just(Token::Whitespace).repeated().ignored();
    let op = |tok| just(tok).padded_by(whitespace.clone());
    let sop = |tok1, tok2| just(tok1).then(just(tok2)).padded_by(whitespace.clone());

    recursive(|bool_expr| {
        let literal = select! {
            Token::True => BoolExpr::True,
            Token::False => BoolExpr::False,
        };

        let atom = choice((
            literal,
            lhs_parser().map(|lhs| BoolExpr::Lvalue(Box::new(lhs))),
            bool_expr.delimited_by(op(Token::LParen), op(Token::RParen)),
            arith_parser()
                .then_ignore(sop(Token::Equals, Token::Equals))
                .then(arith_parser())
                .map(|(a1, a2)| BoolExpr::Eq(Box::new(a1), Box::new(a2))),
            arith_parser()
                .then_ignore(op(Token::LessThan))
                .then(arith_parser())
                .map(|(a1, a2)| BoolExpr::Lt(Box::new(a1), Box::new(a2))),
        ));

        let unary = op(Token::Bang)
            .repeated()
            .foldr(atom, |_op, rhs| BoolExpr::Bang(Box::new(rhs)));

        let wedge = unary.clone().foldl(
            choice((op(Token::And).to(BoolExpr::And as fn(_, _) -> _),))
                .then(unary)
                .repeated(),
            |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
        );
        wedge
    })
    .padded_by(whitespace)
}

pub fn expr_parser<'src, I>() -> impl Parser<'src, I, Expr, extra::Err<Rich<'src, Token>>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    let whitespace = just(Token::Whitespace).repeated().ignored();
    let op = |tok| just(tok).padded_by(whitespace.clone());
    let sop = |tok1, tok2| just(tok1).then(just(tok2)).padded_by(whitespace.clone());

    let variable = select! {
        Token::Var(s) => s
    };

    recursive(
        |expr: Recursive<dyn Parser<'_, I, Expr, extra::Full<Rich<'_, Token>, (), ()>>>| {
            choice((
                lhs_parser().map(|lhs| Expr::Lvalue(Box::new(lhs))),
                expr.clone()
                    .repeated()
                    .collect()
                    .map(|v: Vec<_>| Expr::Tuple(v))
                    .delimited_by(just(Token::LSqBra), just(Token::RSqBra)),
                expr.delimited_by(op(Token::LParen), op(Token::RParen)),
                op(Token::Amp).ignore_then(variable.map(Expr::ImmutRef)),
                sop(Token::Amp, Token::Mut).ignore_then(variable.map(Expr::MutRef)),
                bool_parser().map(|b| Expr::Bool(Box::new(b))),
                arith_parser().map(|a| Expr::Int(Box::new(a))),
            ))
        },
    )
    .padded_by(whitespace)
}

pub fn command_parser<'src, I>() -> impl Parser<'src, I, Cmd, extra::Err<Rich<'src, Token>>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    let whitespace = just(Token::Whitespace).repeated().ignored();
    let op = |tok| just(tok).padded_by(whitespace.clone());

    let variable = select! {
        Token::Var(s) => s
    };

    recursive(
        |cmd: Recursive<dyn Parser<'_, I, Cmd, extra::Full<Rich<'_, Token>, (), ()>>>| {
            let atom = choice((
                op(Token::Skip).to(Cmd::Skip),
                op(Token::Let)
                    .ignore_then(variable)
                    .then_ignore(op(Token::Colon))
                    .then(type_parser())
                    .then_ignore(op(Token::Equals))
                    .then(expr_parser())
                    .map(|((v, t), e)| Cmd::Let(v, Box::new(t), Box::new(e))),
                op(Token::Let)
                    .ignore_then(op(Token::Mut))
                    .ignore_then(variable)
                    .then_ignore(op(Token::Colon))
                    .then(type_parser())
                    .then_ignore(op(Token::Equals))
                    .then(expr_parser())
                    .map(|((v, t), e)| Cmd::LetMut(v, Box::new(t), Box::new(e))),
                op(Token::Let)
                    .ignore_then(variable)
                    .then_ignore(op(Token::Colon))
                    .then(type_parser())
                    .then_ignore(op(Token::Equals))
                    .then_ignore(op(Token::Alloc))
                    .then(arith_parser().delimited_by(op(Token::LParen), op(Token::RParen)))
                    .map(|((v, t), a)| Cmd::LetAlloc(v, Box::new(t), Box::new(a))),
                op(Token::Let)
                    .ignore_then(op(Token::Mut))
                    .ignore_then(variable)
                    .then_ignore(op(Token::Colon))
                    .then(type_parser())
                    .then_ignore(op(Token::Equals))
                    .then_ignore(op(Token::Alloc))
                    .then(arith_parser().delimited_by(op(Token::LParen), op(Token::RParen)))
                    .map(|((v, t), a)| Cmd::LetMutAlloc(v, Box::new(t), Box::new(a))),
                lhs_parser()
                    .then_ignore(op(Token::Equals))
                    .then(expr_parser())
                    .map(|(lhs, e)| Cmd::Assign(Box::new(lhs), Box::new(e))),
                op(Token::Free)
                    .ignore_then(lhs_parser().delimited_by(op(Token::LParen), op(Token::RParen)))
                    .map(|lhs| Cmd::Free(Box::new(lhs))),
                op(Token::While)
                    .ignore_then(bool_parser())
                    .then_ignore(op(Token::Do))
                    .then(cmd.clone())
                    .map(|(b, c)| Cmd::While(Box::new(b), Box::new(c))),
                op(Token::If)
                    .ignore_then(bool_parser())
                    .then_ignore(op(Token::Then))
                    .then(cmd.clone())
                    .then(op(Token::Else).ignore_then(cmd.clone()))
                    .map(|((b, c1), c2)| Cmd::If(Box::new(b), Box::new(c1), Box::new(c2))),
            ));

            atom.clone().foldl(
                choice((op(Token::Semicolon).to(Cmd::Sequence as fn(_, _) -> _),))
                    .then(atom)
                    .repeated(),
                |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
            )
        },
    )
    .padded_by(whitespace)
}
