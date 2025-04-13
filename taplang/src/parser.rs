use crate::ast::{
    Cmd, Decl, Expr, FnDef, HeapPost, HeapPre, HeapPred, ImplFnDef, Lhs, Type, Value,
};
use crate::lexer::Token;
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
        let optional_paren_lhs = choice((
            lhs.clone().delimited_by(
                just(Token::LParen).padded_by(whitespace.clone()),
                just(Token::RParen).padded_by(whitespace.clone()),
            ),
            lhs,
        ));

        let atom = ident;

        let binary = atom
            .or(optional_paren_lhs.clone())
            .foldl(
                just(Token::Dot).ignore_then(num_lit).repeated(),
                |dotted_lhs, i| Lhs::Index(Box::new(dotted_lhs), i),
            )
            .boxed();

        let unary = just(Token::Star)
            .repeated()
            .foldr(binary.or(optional_paren_lhs), |_star, inner| {
                Lhs::Deref(Box::new(inner))
            });

        unary.boxed()
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
            Token::TUnit => Type::Unit,
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

pub fn fn_parser<'src, I>(
    command: Boxed<'src, 'src, I, Cmd, extra::Full<Rich<'src, Token>, (), ()>>,
) -> impl Parser<'src, I, FnDef, extra::Err<Rich<'src, Token>>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    let whitespace = just(Token::Whitespace).repeated().ignored();
    let op = |tok| just(tok).padded_by(whitespace.clone());

    let ident = select! {
        Token::Var(s) => s,
    };
    let args_list = ident
        .then_ignore(op(Token::Colon))
        .then(type_parser())
        .separated_by(op(Token::Comma))
        .collect()
        .delimited_by(op(Token::LParen), op(Token::RParen))
        .boxed();

    let body = command
        .then(
            op(Token::DScolon)
                .ignore_then(op(Token::Return))
                .ignore_then(expr_parser())
                .or_not(),
        )
        .delimited_by(op(Token::LCurBra), op(Token::RCurBra))
        .map(|(commands, maybe_ret)| {
            let ret = maybe_ret.unwrap_or(Expr::Unit);
            (commands, ret)
        })
        .boxed();

    op(Token::FnDef)
        .ignore_then(ident.clone())
        .then(args_list)
        .then_ignore(op(Token::Colon))
        .then(type_parser())
        .boxed()
        .then(body)
        .map(|(((name, args), return_type), (c, e))| {
            FnDef::FnDef(name, args, Box::new(return_type), Box::new(c), Box::new(e))
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

pub fn impl_fn_parser<'src, I>(
    cmd: Boxed<'src, 'src, I, Cmd, extra::Full<Rich<'src, Token>, (), ()>>,
) -> impl Parser<'src, I, ImplFnDef, extra::Err<Rich<'src, Token>>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    let whitespace = just(Token::Whitespace).repeated().ignored();
    let op = |tok| just(tok).padded_by(whitespace.clone());

    let ident = select! {
        Token::Var(s) => s,
    };
    let args_list = ident
        .clone()
        .then_ignore(op(Token::Colon))
        .then(type_parser())
        .separated_by(op(Token::Comma))
        .collect::<Vec<(String, Type)>>()
        .delimited_by(op(Token::LParen), op(Token::RParen))
        .boxed();
    let ret_tau = type_parser().map(|t| Box::new(t));
    let heap_pre = heap_pre_parser().delimited_by(op(Token::LSqBra), op(Token::RSqBra));
    let heap_post = heap_post_parser().delimited_by(op(Token::LSqBra), op(Token::RSqBra));

    let body = cmd
        .then(
            op(Token::DScolon)
                .ignore_then(op(Token::Return))
                .ignore_then(expr_parser())
                .or_not(),
        )
        .delimited_by(op(Token::LCurBra), op(Token::RCurBra))
        .map(|(kcommands, maybe_ret)| {
            let ret = maybe_ret.unwrap_or(Expr::Unit);
            (kcommands, ret)
        })
        .boxed();

    op(Token::FnDef)
        .ignore_then(ident)
        .then(args_list)
        .then_ignore(op(Token::Colon))
        .then(ret_tau)
        .then(heap_pre)
        .then(heap_post)
        .boxed()
        .then(body)
        .map(|(((((name, args), ret), hpre), hpost), (c,e))| ImplFnDef::ImplFnDef(
           name,
            args,
            ret,
            Box::new(hpre),
            Box::new(hpost),
            Box::new(c),
            Box::new(e),
        ))

    // ImplFnDef(String, Vec<(String, Type)>, Box<Type>, Box<HeapPred>, Box<HeapPred>, Box<KCmd>, Box<Expr>)
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

    let num = select! {
        Token::Nat(n) => n
    };

    recursive(
        |expr: Recursive<dyn Parser<'_, I, Expr, extra::Full<Rich<'_, Token>, (), ()>>>| {
            let literal = select! {
                Token::True => Expr::True,
                Token::False => Expr::False,
                Token::Nat(n) => Expr::Nat(n),
            };

            let tuple_expr = expr
                .clone()
                .then_ignore(op(Token::Comma))
                .then(
                    expr.clone()
                        .separated_by(op(Token::Comma))
                        .allow_trailing()
                        .collect(),
                )
                .map(|(first, mut rest)| {
                    let mut elements = vec![first];
                    elements.append(&mut rest);
                    Expr::Tuple(elements)
                })
                .delimited_by(op(Token::LParen), op(Token::RParen));

            let paren_expr = expr
                .clone()
                .delimited_by(op(Token::LParen), op(Token::RParen))
                .boxed();

            let atom = choice((
                literal,
                op(Token::LParen)
                    .ignore_then(op(Token::RParen))
                    .to(Expr::Unit),
                tuple_expr,
                paren_expr,
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
            ))
            .boxed();

            let opt_dot = choice((
                atom.clone()
                    .then_ignore(op(Token::Dot))
                    .then(num)
                    .map(|(e, i)| Expr::Index(Box::new(e), i)),
                atom,
            ));

            let a_unary = op(Token::Minus)
                .repeated()
                .foldr(opt_dot, |_op, rhs| Expr::Neg(Box::new(rhs)));

            let a_product = a_unary.clone().foldl(
                choice((
                    op(Token::Star).to(Expr::Mult as fn(_, _) -> _),
                    op(Token::Divide).to(Expr::Div as fn(_, _) -> _),
                ))
                .then(a_unary)
                .repeated(),
                |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
            );

            let a_sum = a_product
                .clone()
                .foldl(
                    choice((
                        op(Token::Plus).to(Expr::Plus as fn(_, _) -> _),
                        op(Token::Minus).to(Expr::Minus as fn(_, _) -> _),
                    ))
                    .then(a_product)
                    .repeated(),
                    |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
                )
                .boxed();

            let b_atom = choice((
                a_sum
                    .clone()
                    .then_ignore(op(Token::CmpEq))
                    .then(a_sum.clone())
                    .map(|(a1, a2)| Expr::Eq(Box::new(a1), Box::new(a2))),
                a_sum
                    .clone()
                    .then_ignore(op(Token::LessThan))
                    .then(a_sum.clone())
                    .map(|(a1, a2)| Expr::Lt(Box::new(a1), Box::new(a2))),
                a_sum,
            ));

            let b_unary = op(Token::Bang)
                .repeated()
                .foldr(b_atom, |_op, rhs| Expr::Bang(Box::new(rhs)));

            let b_conjdisj = b_unary
                .clone()
                .foldl(
                    choice((
                        op(Token::And).to(Expr::And as fn(_, _) -> _),
                        op(Token::Or).to(Expr::Or as fn(_, _) -> _),
                    ))
                    .then(b_unary)
                    .repeated(),
                    |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
                )
                .boxed();

            b_conjdisj
        },
    )
    .padded_by(whitespace)
}

pub fn decl_parser<'src, I>(
    cmd: Boxed<'src, 'src, I, Cmd, extra::Full<Rich<'src, Token>, (), ()>>,
) -> impl Parser<'src, I, Decl, extra::Err<Rich<'src, Token>>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    let whitespace = just(Token::Whitespace).repeated().ignored();
    let op = |tok| just(tok).padded_by(whitespace.clone());

    let ident = select! {
        Token::Var(s) => s,
    };
    let typedef = op(Token::TypeDef)
        .ignore_then(ident)
        .then_ignore(op(Token::Equals))
        .then(type_parser())
        .map(|(name, tau)| Decl::TypeDef(name, Box::new(tau)));
    let typeimpl = op(Token::Impl)
        .ignore_then(ident)
        .then(heap_pred_parser().delimited_by(op(Token::LSqBra), op(Token::RSqBra)))
        .then(
            impl_fn_parser(cmd)
                .separated_by(op(Token::Comma))
                .collect::<Vec<_>>()
                .delimited_by(op(Token::LCurBra), op(Token::RCurBra)),
        )
        .map(|((name, hp), implfn_vec)| Decl::TypeImpl(name, hp, implfn_vec));
    choice((typedef, typeimpl))
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
            // bool_parser().map(|b| HeapPred::BoolExpr(Box::new(b))),
            hp.clone()
                .delimited_by(op(Token::LParen), op(Token::RParen)),
            op(Token::Hemp).to(HeapPred::Emp),
            ident
                .then_ignore(op(Token::Hpointsto))
                .then(value_parser())
                .map(|(s, v)| HeapPred::Pointsto(s, Box::new(v))),
            ident.map(|s| HeapPred::Var(s)),
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
        atom.boxed()
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
            let skip = op(Token::Skip).to(Cmd::Skip);
            let scope = cmd
                .clone()
                .delimited_by(op(Token::LCurBra), op(Token::RCurBra))
                .map(|c| Cmd::Scope(Box::new(c)));
            let declaration =
                decl_parser(cmd.clone().boxed()).map(|d: Decl| Cmd::TypeDecl(Box::new(d)));

            let a: Boxed<'_, '_, I, Cmd, extra::Full<Rich<'_, Token>, (), ()>> =
                cmd.clone().boxed();
            let fxn = fn_parser(a).map(|f| Cmd::FxnDefin(Box::new(f)));
            let letass = op(Token::Let)
                .ignore_then(ident)
                .then_ignore(op(Token::Colon))
                .then(type_parser())
                .then_ignore(op(Token::Equals))
                .then(expr_parser())
                .map(|((v, t), e)| Cmd::Let(v, Box::new(t), Box::new(e)));
            let letmutass = op(Token::Let)
                .ignore_then(op(Token::Mut))
                .ignore_then(ident)
                .then_ignore(op(Token::Colon))
                .then(type_parser())
                .then_ignore(op(Token::Equals))
                .then(expr_parser())
                .map(|((v, t), e)| Cmd::LetMut(v, Box::new(t), Box::new(e)));
            let ass = lhs_parser()
                .then_ignore(op(Token::Equals))
                .then(expr_parser())
                .map(|(lhs, e)| Cmd::Assign(Box::new(lhs), Box::new(e)));
            let whilestmt = op(Token::While)
                .ignore_then(expr_parser())
                .then(scope.clone())
                .map(|(b, c)| Cmd::While(Box::new(b), Box::new(c)));
            let ifstmt = op(Token::If)
                .ignore_then(expr_parser())
                .then(scope.clone())
                .then_ignore(op(Token::Else))
                .then(scope.clone())
                .map(|((b, c1), c2)| Cmd::If(Box::new(b), Box::new(c1), Box::new(c2)));
            let lemma = op(Token::Lemma)
                .ignore_then(heap_pred_parser())
                .map(|h| Cmd::Lemma(Box::new(h)));

            let atom = choice((
                skip,
                scope,
                declaration,
                fxn,
                letass,
                letmutass,
                whilestmt,
                ifstmt,
                ass,
                lemma
            ));

            let semicolon_op = op(Token::Semicolon).to(Cmd::Sequence as fn(_, _) -> _);

            let sequence = atom
                .clone()
                .foldl(semicolon_op.then(atom).repeated(), |lhs, (op, rhs)| {
                    op(Box::new(lhs), Box::new(rhs))
                })
                .boxed();

            sequence
        },
    )
    .padded_by(whitespace)
}
