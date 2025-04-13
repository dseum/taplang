use crate::ast::{
    Cmd, Decl, Expr, FnDef, HeapPost, HeapPre, HeapPred, ImplFnDef, KCmd, Lhs, Type, Value,
};
use crate::lexer::Token;
use chumsky::combinator::To;
use chumsky::{input::ValueInput, prelude::*};

pub fn lhs_parser<'src, I>() -> impl Parser<'src, I, Lhs, extra::Err<Rich<'src, Token>>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    let num_lit = select! {
        Token::Nat(n) => n,
    };
    let ident = select! {
        Token::Var(s) => Lhs::Var(s),
    };

    let whitespace = just(Token::Whitespace).repeated().ignored();

    recursive(|lhs| {
        choice((
            ident,
            lhs.clone().delimited_by(
                just(Token::LParen).padded_by(whitespace.clone()),
                just(Token::RParen).padded_by(whitespace.clone()),
            ),
            lhs.clone().foldl(
                just(Token::Dot).ignore_then(num_lit).repeated(),
                |dotted_lhs, i| Lhs::Index(Box::new(dotted_lhs), i),
            ),
            just(Token::Star)
                .repeated()
                .foldr(lhs, |_star, inner| Lhs::Deref(Box::new(inner))),
        ))
    })
    .padded_by(whitespace)
}

pub fn type_parser<'src, I>() -> impl Parser<'src, I, Type, extra::Err<Rich<'src, Token>>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    let whitespace = just(Token::Whitespace).repeated().ignored();
    let op = |tok| just(tok).padded_by(whitespace.clone());

    recursive(|typeshi| {
        let atom = select! {
            Token::TInt => Type::Int,
            Token::TBool => Type::Bool,
            Token::TUnit => (Type::Unit),
            Token::Var(s) => Type::CustomType(s),
        };
        choice((
            atom,
            typeshi
                .clone()
                .delimited_by(op(Token::LParen), op(Token::RParen)),
            op(Token::TRef)
                .ignore_then(op(Token::LessThan))
                .ignore_then(typeshi.clone())
                .then_ignore(op(Token::GreaterThan))
                .map(|tau| Type::Ref(Box::new(tau))),
            op(Token::TRefMut)
                .ignore_then(op(Token::LessThan))
                .ignore_then(typeshi.clone())
                .then_ignore(op(Token::GreaterThan))
                .map(|tau| Type::RefMut(Box::new(tau))),
            op(Token::TLoc)
                .ignore_then(op(Token::LessThan))
                .ignore_then(typeshi.clone())
                .then_ignore(op(Token::GreaterThan))
                .map(|tau| Type::Loc(Box::new(tau))),
            typeshi
                .clone()
                .separated_by(op(Token::Comma))
                .allow_trailing()
                .collect()
                .delimited_by(op(Token::LParen), op(Token::RParen))
                .map(|v| Type::Prod(v)),
        ))
        .padded_by(whitespace.clone())
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
        ));

        let unary = op(Token::Minus)
            .repeated()
            .foldr(atom, |_op, rhs| ArithExpr::Neg(Box::new(rhs)));

        let product = unary.clone().foldl(
            choice((
                op(Token::Star).to(ArithExpr::Mult as fn(_, _) -> _),
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
            choice((
                op(Token::And).to(BoolExpr::And as fn(_, _) -> _),
                op(Token::Or).to(BoolExpr::Or as fn(_, _) -> _),
            ))
            .then(unary)
            .repeated(),
            |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
        );
        wedge
    })
    .padded_by(whitespace)
}

pub fn fn_parser<'src, I>() -> impl Parser<'src, I, FnDef, extra::Err<Rich<'src, Token>>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    let whitespace = just(Token::Whitespace).repeated().ignored();
    let op = |tok| just(tok).padded_by(whitespace.clone());

    let ident = select! {
        Token::Var(s) => s,
    };

    op(Token::FnDef)
        .ignore_then(ident.clone())
        .then(
            ident
                .then_ignore(op(Token::Colon))
                .then(type_parser())
                .separated_by(op(Token::Comma))
                .collect()
                .delimited_by(op(Token::LParen), op(Token::RParen)),
        )
        .then_ignore(op(Token::Colon))
        .then(type_parser())
        .then(
            command_parser()
                .then(
                    op(Token::Semicolon)
                        .ignore_then(op(Token::Return))
                        .ignore_then(expr_parser()),
                )
                .delimited_by(op(Token::LCurBra), op(Token::RCurBra)),
        )
        .map(|(((name, args), return_type), (body, e))| {
            FnDef::FnDef(
                name,
                args,
                Box::new(return_type),
                Box::new(body),
                Box::new(e),
            )
        })
}

pub fn heap_pre_parser<'src, I>()
-> impl Parser<'src, I, HeapPre, extra::Err<Rich<'src, Token>>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    let whitespace = just(Token::Whitespace).repeated().ignored();
    let op = |tok| just(tok).padded_by(whitespace);

    choice((
        op(Token::Pre)
            .ignore_then(heap_pred_parser())
            .map(|hp| HeapPre::Pre(Box::new(hp))),
        empty().to(HeapPre::Vacuous),
    ))
}

pub fn heap_post_parser<'src, I>()
-> impl Parser<'src, I, HeapPost, extra::Err<Rich<'src, Token>>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    let whitespace = just(Token::Whitespace).repeated().ignored();
    let op = |tok| just(tok).padded_by(whitespace);

    choice((
        op(Token::Post)
            .ignore_then(heap_pred_parser())
            .map(|hp| HeapPost::Post(Box::new(hp))),
        empty().to(HeapPost::Vacuous),
    ))
}

pub fn impl_fn_parser<'src, I>()
-> impl Parser<'src, I, ImplFnDef, extra::Err<Rich<'src, Token>>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    // ImplFnDef(String, Vec<(String, Type)>, Box<Type>, Box<HeapPred>, Box<HeapPred>, Box<KCmd>, Box<Expr>)

    let whitespace = just(Token::Whitespace).repeated().ignored();
    let op = |tok| just(tok).padded_by(whitespace.clone());

    let ident = select! {
        Token::Var(s) => s,
    };

    // op(Token::FnDef)
    //     .ignore_then(ident.clone())
    //     .then(
    //         ident
    //             .then_ignore(op(Token::Colon))
    //             .then(type_parser())
    //             .separated_by(op(Token::Comma))
    //             .collect()
    //             .delimited_by(op(Token::LParen), op(Token::RParen)),
    //     )
    //     .then_ignore(op(Token::Colon))
    //     .then(type_parser())
    //     .then(heap_pre_parser().delimited_by(op(Token::LSqBra), op(Token::RSqBra)))
    //     .then(heap_post_parser().delimited_by(op(Token::LSqBra), op(Token::RSqBra)))
    //     .then(
    //         k_command_parser()
    //             .then(
    //                 op(Token::Semicolon)
    //                     .ignore_then(op(Token::Return))
    //                     .ignore_then(expr_parser()),
    //             )
    //             .delimited_by(op(Token::LCurBra), op(Token::RCurBra)),
    //     )
    //     .map(
    //         |(((((name, args), return_type), hpre), hpost), (body, e))| {
    //             ImplFnDef::ImplFnDef(
    //                 name,
    //                 args,
    //                 Box::new(return_type),
    //                 Box::new(hpre),
    //                 Box::new(hpost),
    //                 Box::new(body),
    //                 Box::new(e),
    //             )
    //         },
    //     )

    op(Token::FnDef)
        .ignore_then(ident.clone())
        .then(
            ident
                .then_ignore(op(Token::Colon))
                .then(type_parser())
                .separated_by(op(Token::Comma))
                .collect()
                .delimited_by(op(Token::LParen), op(Token::RParen)),
        )
        .then_ignore(op(Token::Colon))
        .then(type_parser())
        .then(heap_pre_parser().delimited_by(op(Token::LSqBra), op(Token::RSqBra)))
        // .then(heap_post_parser().delimited_by(op(Token::LSqBra), op(Token::RSqBra)))
        // .then(
        //     k_command_parser()
        //         .then(
        //             op(Token::Semicolon)
        //                 .ignore_then(op(Token::Return))
        //                 .ignore_then(expr_parser()),
        //         )
        //         .delimited_by(op(Token::LCurBra), op(Token::RCurBra)),
        // )
        .map(|(((name, args), return_type), heap_pre)| {
            ImplFnDef::ImplFnDef(
                name,
                args,
                Box::new(return_type),
                Box::new(heap_pre),
                Box::new(HeapPost::Vacuous),
                Box::new(KCmd::Command(Box::new(Cmd::Skip))),
                Box::new(Expr::Unit),
            )
        })
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
                variable
                    .then(variable)
                    .then(
                        expr.clone()
                            .separated_by(op(Token::Comma))
                            .collect()
                            .delimited_by(op(Token::LParen), op(Token::RParen)),
                    )
                    .map(|((s, f), e)| Expr::BoundCall(s, f, e)),
                variable
                    .then(
                        expr.clone()
                            .separated_by(op(Token::Comma))
                            .collect()
                            .delimited_by(op(Token::LParen), op(Token::RParen)),
                    )
                    .map(|(s, e)| Expr::Call(s, e)),
                lhs_parser().map(|lhs| Expr::Lvalue(Box::new(lhs))),
                op(Token::LParen)
                    .ignore_then(op(Token::RParen))
                    .to(Expr::Unit),
                expr.clone()
                    .delimited_by(op(Token::LParen), op(Token::RParen)),
                expr.clone()
                    .separated_by(op(Token::Comma))
                    .at_least(1)
                    .allow_trailing()
                    .collect()
                    .map(|v: Vec<_>| Expr::Tuple(v))
                    .delimited_by(op(Token::LParen), op(Token::RParen)),
                op(Token::Alloc)
                    .ignore_then(
                        expr.clone()
                            .delimited_by(op(Token::LParen), op(Token::RParen)),
                    )
                    .map(|e: Expr| Expr::Alloc(Box::new(e))),
                op(Token::Free)
                    .ignore_then(lhs_parser().delimited_by(op(Token::LParen), op(Token::RParen)))
                    .map(|l: Lhs| Expr::Free(Box::new(l))),
                op(Token::Amp).ignore_then(variable.map(Expr::ImmutRef)),
                sop(Token::Amp, Token::Mut).ignore_then(variable.map(Expr::MutRef)),
                bool_parser().map(|b| Expr::Bool(Box::new(b))),
                arith_parser().map(|a| Expr::Int(Box::new(a))),
            ))
        },
    )
    .padded_by(whitespace)
}

pub fn decl_parser<'src, I>() -> impl Parser<'src, I, Decl, extra::Err<Rich<'src, Token>>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    let whitespace = just(Token::Whitespace).repeated().ignored();
    let op = |tok| just(tok).padded_by(whitespace.clone());

    let ident = select! {
        Token::Var(s) => s,
    };
    choice((
        // op(Token::TypeDef)
        //     .ignore_then(ident)
        //     .then_ignore(op(Token::Equals))
        //     .then(type_parser())
        //     .map(|(name, tau)| Decl::TypeDef(name, Box::new(tau))),
        op(Token::Impl)
            .ignore_then(ident)
            .then(heap_pred_parser().delimited_by(op(Token::LCurBra), op(Token::RCurBra)))
            .then(impl_fn_parser().separated_by(op(Token::Comma)).collect())
            .map(|((name, hp), fns)| Decl::TypeImpl(name, hp, fns)),
    ))
}
pub fn value_parser<'src, I>() -> impl Parser<'src, I, Value, extra::Err<Rich<'src, Token>>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    let whitespace = just(Token::Whitespace).repeated().ignored();
    let op = |tok| just(tok).padded_by(whitespace.clone());
    let nat = select! {
        Token::Nat(n) => n,
    };
    choice((
        op(Token::LParen)
            .ignore_then(op(Token::RParen))
            .to(Value::Unit),
        op(Token::True).to(Value::True),
        op(Token::False).to(Value::False),
        nat.map(|n| Value::Int(n)),
        op(Token::Minus).ignore_then(nat).map(|n| Value::Int(-n)),
    ))
}

pub fn heap_pred_parser<'src, I>()
-> impl Parser<'src, I, HeapPred, extra::Err<Rich<'src, Token>>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    let whitespace = just(Token::Whitespace).repeated().ignored();
    let op = |tok| just(tok).padded_by(whitespace.clone());

    let ident = select! {
        Token::Var(s) => s,
    };
    recursive(|hp| {
        let atom = choice((
            bool_parser().map(|b| HeapPred::BoolExpr(Box::new(b))),
            hp.clone()
                .delimited_by(op(Token::LParen), op(Token::RParen)),
            op(Token::Hemp).to(HeapPred::Emp),
            ident
                .then_ignore(op(Token::Hpointsto))
                .then(value_parser())
                .map(|(s, v)| HeapPred::Pointsto(s, Box::new(v))),
        ));

        // let unary = op(Token::Hnot)
        //     .repeated()
        //     .foldr(atom, |_op, inner_hp| HeapPred::Not(Box::new(inner_hp)));

        // let conj = unary.clone().foldl(
        //     choice((op(Token::Hand).to(HeapPred::And as fn(_, _) -> _),))
        //         .then(unary)
        //         .repeated(),
        //     |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
        // );

        // let disj = conj.clone().foldl(
        //     choice((op(Token::Hor).to(HeapPred::Or as fn(_, _) -> _),))
        //         .then(conj)
        //         .repeated(),
        //     |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
        // );

        // let exp = disj.clone().foldl(
        //     choice((op(Token::Himplies).to(HeapPred::Implies as fn(_, _) -> _),))
        //         .then(disj)
        //         .repeated(),
        //     |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
        // );

        // let sepconj = exp.clone().foldl(
        //     choice((op(Token::Hsepconj).to(HeapPred::SepConj as fn(_, _) -> _),))
        //         .then(exp)
        //         .repeated(),
        //     |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
        // );

        // let magicwand = sepconj.clone().foldl(
        //     choice((op(Token::Hmagicwand).to(HeapPred::MagicWand as fn(_, _) -> _),))
        //         .then(sepconj)
        //         .repeated(),
        //     |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
        // );

        // let quantifier = choice((op(Token::Hforall).to(HeapPred::Forall as fn(_, _) -> _),
        // op(Token::Hexists).to(HeapPred::Exists as fn(_, _) -> _))).then(ident).repeated()
        //     .foldr(magicwand, |(f, s), hp| f(s,  Box::new(hp)));

        // op(Token::Hexists)
        // .ignore_then(ident)
        // .then(hp.clone())
        // .map(|(s, inner_hp)| HeapPred::Exists(s, Box::new(inner_hp))),

        // choice((
        //     op(Token::Hforall)
        //         .ignore_then(ident)
        //         .then(hp.clone())
        //         .map(|(s, inner_hp)| HeapPred::Forall(s, Box::new(inner_hp))),
        //     magicwand,
        // ))
        atom
    })
}

pub fn command_parser<'src, I>() -> impl Parser<'src, I, Cmd, extra::Err<Rich<'src, Token>>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    let whitespace = just(Token::Whitespace).repeated().ignored();
    let op = |tok| just(tok).padded_by(whitespace.clone());

    let ident = select! {
        Token::Var(s) => s
    };

    recursive(
        |cmd: Recursive<dyn Parser<'_, I, Cmd, extra::Full<Rich<'_, Token>, (), ()>>>| {
            let atom = choice((
                decl_parser().map(|d| Cmd::TypeDecl(Box::new(d))),
                fn_parser().map(|f| Cmd::FxnDefin(Box::new(f))),
                cmd.clone()
                    .delimited_by(op(Token::LCurBra), op(Token::RCurBra))
                    .map(|c| Cmd::Scope(Box::new(c))),
                op(Token::Skip).to(Cmd::Skip),
                op(Token::Let)
                    .ignore_then(ident)
                    .then_ignore(op(Token::Colon))
                    .then(type_parser())
                    .then_ignore(op(Token::Equals))
                    .then(expr_parser())
                    .map(|((v, t), e)| Cmd::Let(v, Box::new(t), Box::new(e))),
                op(Token::Let)
                    .ignore_then(op(Token::Mut))
                    .ignore_then(ident)
                    .then_ignore(op(Token::Colon))
                    .then(type_parser())
                    .then_ignore(op(Token::Equals))
                    .then(expr_parser())
                    .map(|((v, t), e)| Cmd::LetMut(v, Box::new(t), Box::new(e))),
                lhs_parser()
                    .then_ignore(op(Token::Equals))
                    .then(expr_parser())
                    .map(|(lhs, e)| Cmd::Assign(Box::new(lhs), Box::new(e))),
                op(Token::Free)
                    .ignore_then(lhs_parser().delimited_by(op(Token::LParen), op(Token::RParen)))
                    .map(|lhs| Cmd::Free(Box::new(lhs))),
                op(Token::While)
                    .ignore_then(bool_parser())
                    .then(
                        cmd.clone()
                            .delimited_by(op(Token::LCurBra), op(Token::RCurBra))
                            .map(|c| Cmd::Scope(Box::new(c))),
                    )
                    .map(|(b, c)| Cmd::While(Box::new(b), Box::new(c))),
                op(Token::If)
                    .ignore_then(bool_parser())
                    .then(
                        cmd.clone()
                            .delimited_by(op(Token::LCurBra), op(Token::RCurBra))
                            .map(|c| Cmd::Scope(Box::new(c))),
                    )
                    .then(
                        op(Token::Else).ignore_then(
                            cmd.clone()
                                .delimited_by(op(Token::LCurBra), op(Token::RCurBra))
                                .map(|c| Cmd::Scope(Box::new(c))),
                        ),
                    )
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

pub fn k_command_parser<'src, I>()
-> impl Parser<'src, I, KCmd, extra::Err<Rich<'src, Token>>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    let whitespace = just(Token::Whitespace).repeated().ignored();
    let op = |tok| just(tok).padded_by(whitespace.clone());

    recursive(
        |kcmd: Recursive<dyn Parser<'_, I, KCmd, extra::Full<Rich<'_, Token>, (), ()>>>| {
            choice((
                command_parser().map(|c| KCmd::Command(Box::new(c))),
                kcmd.clone()
                    .then_ignore(op(Token::Semicolon))
                    .then(kcmd.clone())
                    .map(|(k1, k2)| KCmd::Sequence(Box::new(k1), Box::new(k2))),
                op(Token::Lemma)
                    .ignore_then(heap_pred_parser())
                    .map(|h| KCmd::Lemma(Box::new(h))),
            ))
        },
    )
}
