use std::collections::VecDeque;

use crate::ast::{
    Cmd, Decl, Expr, FnDef, HeapPost, HeapPre, HeapPred, ImplFnDef, KCmd, KExpr, Lhs, Span,
    Spanned, Type,
};
use crate::lexer::Token;

#[derive(Debug)]
pub struct ParseError {
    span: Span,
    message: String,
    src: String,
    ctx: String,
}

impl ParseError {
    fn excerpt(source: &str, span: &Span) -> String {
        let mut buffer = String::new();

        let mut line_start = 0;
        for (i, line) in source.lines().enumerate() {
            let line_end = line_start + line.len();

            // Check if this line overlaps the span
            if span.end > line_start && span.start < line_end {
                let line_num = i + 1;

                let start_col = span.start.saturating_sub(line_start);
                let end_col = (span.end - line_start).min(line.len());

                buffer.push_str(&format!("{:>4} | {}\n     | ", line_num, line));
                if start_col == end_col {
                    for _ in 0..line.len() {
                        buffer.push_str(" ");
                    }
                    buffer.push_str("^");
                } else {
                    for i in 0..line.len() {
                        if i >= start_col && i < end_col {
                            buffer.push_str("^");
                        } else {
                            buffer.push_str(" ");
                        }
                    }
                }
                buffer.push_str("\n");
            }

            // Move to next line
            line_start = line_end + 1; // +1 for the newline
        }
        buffer
    }

    pub fn to_string(&self) -> String {
        let header = format!(
            "While attempting to {},
            {}\nError was triggered at ({}..{}):",
            self.ctx, self.message, self.span.start, self.span.end
        );
        let code = Self::excerpt(&self.src, &self.span);
        format!("{header}\n{code}")
    }

    pub fn msg(&self) {
        println!("{}", self.to_string());
    }
}

pub struct Parser<'src> {
    src: &'src String,
    tokens: &'src mut VecDeque<Spanned<Token>>,
    left_span: Span,
}
impl<'src> Parser<'src> {
    pub fn new(
        src: &'src String,
        tokens: &'src mut VecDeque<Spanned<Token>>,
        left_span: Span,
    ) -> Self {
        Self {
            src,
            tokens,
            left_span,
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.front().map(|tw| &tw.0)
    }

    fn next(&mut self, ctx: String) -> Result<Spanned<Token>, ParseError> {
        if let Some(tok) = self.tokens.pop_front() {
            self.left_span = tok.1.end..self.left_span.end;
            Ok(tok)
        } else {
            Err(ParseError {
                src: format!("{} ", self.src.clone()),
                span: self.left_span.clone(),
                message: format!("Unexpected EOF."),
                ctx,
            })
        }
    }

    fn unexpected(
        &self,
        ctx: String,
        unexpected_token: Token,
    ) -> Result<Spanned<Token>, ParseError> {
        Err(ParseError {
            src: self.src.clone(),
            span: self.left_span.clone(),
            message: format!("Unexpected `{}`.", unexpected_token),
            ctx,
        })
    }

    fn expect(&mut self, expected: Token, ctx: String) -> Result<Spanned<Token>, ParseError> {
        let (next_tok, tok_span) = self.next(ctx.clone())?;

        // self.next(...) modifies self.left_span, so we can take advantage of the mutation
        if std::mem::discriminant(&expected) == std::mem::discriminant(&next_tok) {
            Ok((next_tok, tok_span))
        } else {
            Err(ParseError {
                src: self.src.clone(),
                span: tok_span,
                message: format!("Expected `{}`, but found `{}`", expected, next_tok),
                ctx,
            })
        }
    }

    fn expect_plur(
        &mut self,
        expected_tok_seq: Vec<Token>,
        ctx: String,
    ) -> Result<Vec<Spanned<Token>>, ParseError> {
        let mut spanned_toks_vec = Vec::<Spanned<Token>>::new();
        for expected in expected_tok_seq.into_iter() {
            spanned_toks_vec.push(self.expect(expected, ctx.clone())?);
        }
        Ok((spanned_toks_vec))
    }

    fn metaparse_separated_by<T>(
        &mut self,
        inner: fn(&mut Self) -> Result<Spanned<T>, ParseError>,
        sep: Token,
        allow_trailing_sep: bool,
        min_sep_occur: i32,
    ) -> Result<Vec<Spanned<T>>, ParseError> {
        let span_start = self.left_span.start;
        let mut v = Vec::new();

        // if true, parse inner. else, sep
        let mut sep_or_inner = true;

        let mut sep_count = 0;

        while self.tokens.len() > 0 {
            let mut tok_cop = self.tokens.clone();
            if sep_or_inner {
                if let Ok(foo) = inner(self) {
                    v.push(foo)
                } else {
                    self.tokens.clear();
                    self.tokens.append(&mut tok_cop);
                    break;
                }
            } else {
                if self
                    .expect(sep.clone(), format!("parse separator {}", sep.clone()))
                    .is_err()
                {
                    self.tokens.clear();
                    self.tokens.append(&mut tok_cop);
                    break;
                } else {
                    sep_count += 1
                }
            }
            sep_or_inner = !sep_or_inner;
        }

        if allow_trailing_sep {
            if self.lookahead_match_tokens(&vec![sep.clone()]) {
                sep_count += 1;
                self.expect(sep.clone(), "this shouldn't fail lmao".to_string())?;
            }
        }

        let span_end = self.left_span.start;

        if sep_count < min_sep_occur {
            Err(ParseError {
                src: self.src.clone(),
                span: span_start..span_end,
                message: format!(
                    "Expected `{}` many separators, but only found `{}` many separators",
                    min_sep_occur, sep_count
                ),
                ctx: format!("parse a separated pattern with separator {}", sep),
            })
        } else {
            Ok(v)
        }
    }

    fn metaparse_or<T>(
        &mut self,
        fxns: Vec<fn(&mut Self) -> Result<Spanned<T>, ParseError>>,
        ctx: String,
    ) -> Result<Spanned<T>, ParseError> {
        let mut errs = Vec::new();
        let n = fxns.len();
        for f in fxns.into_iter() {
            let tokens_cop = self.tokens.clone();
            match f(self) {
                Ok(spanned_expr1) => return Ok(spanned_expr1),
                Err(e) => errs.push(e),
            }
            self.tokens.clear();
            self.tokens.extend(tokens_cop);
        }

        let min_span_start = errs
            .iter()
            .map(|pe| pe.span.start)
            .min()
            .expect("(internal-error) this function should never be called with an empty fxns");
        let max_span_end = errs
            .iter()
            .map(|pe| pe.span.end)
            .max()
            .expect("(internal-error) this function should never be called with an empty fxns");

        Err(ParseError {
                    src: self.src.clone(),
                    span: (min_span_start..max_span_end),
                    message: format!("parse {n} possible grammar rules, but all failed. Here are their individual messages:
                {}", errs.iter().map(ParseError::to_string).collect::<Vec<String>>().join("\n\n")),
                    ctx
                })
    }

    fn macroparse_operator_unary_prefix_hom<T>(
        &mut self,
        uops: Vec<(Token, fn(Box<Spanned<T>>) -> T)>,
        parse_inside: fn(&mut Self) -> Result<Spanned<T>, ParseError>,
        ctx: String,
        start_span: Span,
    ) -> Result<Spanned<T>, ParseError> {
        // let (uop_tok, uop_span) = self.next(ctx.clone())?;

        let span_start = self.left_span.start;
        let mut fxns = Vec::new();
        while self.tokens.len() > 0 {
            let mut is_handled = false;
            for (uop_tok, uop_f) in uops.iter() {
                if self.lookahead_match_tokens(&vec![uop_tok.clone()]) {
                    let (_, t_span) = self.expect(
                        uop_tok.clone(),
                        "(internal-error) this should never fail".to_string(),
                    )?;
                    fxns.push((uop_f, t_span));
                    is_handled = true;
                    break;
                }
            }
            if !is_handled {
                break;
            }
        }
        let (insides, insides_span) = parse_inside(self)?;
        let span_end = insides_span.end;
        Ok((
            // reversed order to bind inner-most first
            fxns.into_iter()
                .rev()
                .fold(insides, |acc, (f, span)| f(Box::new((acc, span)))),
            span_start..span_end,
        ))
    }

    fn macroparse_operator_binary_infix_left_assoc<T: Clone, A: Clone>(
        &mut self,
        bops: Vec<(Token, fn(Box<Spanned<T>>, Box<Spanned<A>>) -> T)>,
        parse_lhs: fn(&mut Self) -> Result<Spanned<T>, ParseError>,
        parse_rhs: fn(&mut Self) -> Result<Spanned<A>, ParseError>,
        ctx: String,
        start_span: Span,
    ) -> Result<Spanned<T>, ParseError> {
        let spanned_e = parse_lhs(self)?;

        let mut curr = spanned_e;

        while self.tokens.len() > 0 {
            let mut is_handled = false;
            for (bop_poss_tok, bop_poss_fn) in bops.iter() {
                if self.lookahead_match_tokens(&vec![bop_poss_tok.clone()]) {
                    self.expect(bop_poss_tok.clone(), ctx.clone())?;
                    let (inner, inner_span) = parse_rhs(self)?;
                    curr = (
                        bop_poss_fn(
                            Box::new(curr.clone()),
                            Box::new((inner, inner_span.clone())),
                        ),
                        start_span.start..inner_span.end,
                    );
                    is_handled = true;
                    break;
                }
            }
            if !is_handled {
                break;
            }
        }
        Ok(curr)
    }

    fn lookahead_match_tokens(&self, toks: &Vec<Token>) -> bool {
        toks.iter()
            .enumerate()
            .all(|(i, tok)| match self.tokens.get(i) {
                Some((next, _)) => std::mem::discriminant(next) == std::mem::discriminant(tok),
                _ => false,
            })
    }

    // accepts subvecs but the order of appearance is fixed. consider:
    // [3, 5, 1] <= [3, 1, 5, 2, 1]   IS false
    // [3, 5, 1] <= [3, 2, 5, 2, 2, 2, 1, 0]   IS true
    fn lookahead_match_tokens_spaced_out_strict_order(&self, target_toks: &Vec<Token>) -> bool {
        let mut haystack_pos = 0;

        for item in target_toks {
            // Search for the next matching item starting from haystack_pos
            if let Some(pos) = self
                .tokens
                .iter()
                .skip(haystack_pos)
                .position(|(h, _)| h == item)
            {
                haystack_pos += pos + 1; // move past the found item
            } else {
                return false; // not found
            }
        }

        return true;
    }

    // accepts subvecs
    fn lookahead_match_tokens_spaced_out(&self, target_toks: &Vec<Token>) -> bool {
        let mut it = self.tokens.iter();
        for targ_tok in target_toks {
            let mut targ_tok_found = false;
            for (next, _) in &mut it {
                if std::mem::discriminant(next) == std::mem::discriminant(targ_tok) {
                    targ_tok_found = true;
                    break;
                }
            }
            if !targ_tok_found {
                return false;
            }
        }
        return true;
    }

    fn macroparse_optional_wrap_delimiters<T>(
        &mut self,
        parse: fn(&mut Self) -> Result<Spanned<T>, ParseError>,
        left_delimiter: Token,
        right_delimiter: Token,
    ) -> Result<(Spanned<T>, Span), ParseError> {
        if self.lookahead_match_tokens(&vec![left_delimiter.clone()]) {
            self.macroparse_wrap_delimiters(parse, left_delimiter, right_delimiter)
        } else {
            let (innie, innie_span) = parse(self)?;
            Ok(((innie, innie_span.clone()), innie_span))
        }
    }

    fn macroparse_wrap_delimiters<T>(
        &mut self,
        parse: fn(&mut Self) -> Result<T, ParseError>,
        left_delimiter: Token,
        right_delimiter: Token,
    ) -> Result<(T, Span), ParseError> {
        let (_, lcurbra_span) = self.expect(
            left_delimiter.clone(),
            format!(
                "parse a {} {} delimted grammatical unit, but failed to open",
                left_delimiter.clone(),
                right_delimiter.clone()
            ),
        )?;

        let spanned_inner = parse(self)?;

        let (_, rcurbra_span) = self.expect(
            right_delimiter.clone(),
            format!(
                "parse a {} {} delimted grammatical unit, but failed to close",
                left_delimiter, right_delimiter
            ),
        )?;
        Ok((spanned_inner, lcurbra_span.start..rcurbra_span.end))
    }

    fn parse_lhs_name(&mut self) -> Result<Spanned<Lhs<()>>, ParseError> {
        match self.expect(Token::Var(String::new()), "parse a name".to_string())? {
            (Token::Var(name), span) => Ok((Lhs::Var(name, ()), span)),
            _ => panic!(),
        }
    }

    fn parse_lhs_atom(&mut self) -> Result<Spanned<Lhs<()>>, ParseError> {
        if self.lookahead_match_tokens(&vec![Token::LParen]) {
            let ((lhs, _lhs_span), delim_lhs_span) =
                self.macroparse_wrap_delimiters(Self::parse_lhs, Token::LParen, Token::RParen)?;
            Ok((lhs, delim_lhs_span))
        } else {
            self.parse_lhs_name()
        }
    }

    fn parse_lhs_deref(&mut self) -> Result<Spanned<Lhs<()>>, ParseError> {
        let uops = vec![
            (
                Token::Star,
                (|l| Lhs::DerefPointer(l, ())) as fn(Box<Spanned<Lhs<()>>>) -> Lhs<()>,
            ),
            (
                Token::At,
                (|l| Lhs::DerefRef(l, ())) as fn(Box<Spanned<Lhs<()>>>) -> Lhs<()>,
            ),
        ];
        self.macroparse_operator_unary_prefix_hom(
            uops,
            Self::parse_lhs,
            "parse an lhs_dereference".to_string(),
            self.left_span.clone(),
        )
    }

    fn parse_lhs_index(&mut self) -> Result<Spanned<Lhs<()>>, ParseError> {
        let parse_index = |s: &mut Self| match s.expect(
            Token::Nat(0),
            "parse an index -- a natural number".to_string(),
        )? {
            (Token::Nat(n), sp) => Ok((n, sp)),
            _ => panic!(),
        };

        let bops: Vec<(
            Token,
            fn(
                Box<(Lhs<()>, std::ops::Range<usize>)>,
                Box<(isize, std::ops::Range<usize>)>,
            ) -> Lhs<()>,
        )> = vec![(
            Token::Dot,
            (|l, i| Lhs::Index(l, (*i).0, ()))
                as fn(Box<Spanned<Lhs<()>>>, Box<(isize, std::ops::Range<usize>)>) -> Lhs<()>,
        )];
        self.macroparse_operator_binary_infix_left_assoc(
            bops,
            Self::parse_lhs_atom,
            parse_index,
            "parse an lhs_index".to_string(),
            self.left_span.clone(),
        )
    }

    pub fn parse_lhs(&mut self) -> Result<Spanned<Lhs<()>>, ParseError> {
        if self.lookahead_match_tokens(&vec![Token::At])
            || self.lookahead_match_tokens(&vec![Token::Star])
        {
            self.parse_lhs_deref()
        } else {
            self.parse_lhs_index()
        }
    }

    fn lookahead_is_lhs(&self) -> bool {
        let mut toks = self.tokens.clone();

        let mut p = Parser {
            src: self.src,
            tokens: &mut toks,
            left_span: self.left_span.clone(),
        };

        p.parse_lhs().is_ok()
    }

    fn parse_expr_val(&mut self) -> Result<Spanned<Expr<()>>, ParseError> {
        match self.next("parse a value literal".to_string())? {
            (Token::LParen, slparen) => {
                let (_, srparen) = self.expect(Token::RParen, "parse a unit".to_string())?;
                Ok((Expr::Unit(()), slparen.start..srparen.end))
            }
            (Token::True, strue) => Ok((Expr::True(()), strue)),
            (Token::False, sfalse) => Ok((Expr::False(()), sfalse)),
            (Token::Nat(n), snat) => Ok((Expr::Nat(n, ()), snat)),
            (unexp_tok, unexp_span) => {
                let message = format!(
                    "Expected one of `()`, `true`, `false`, or r`\\d+`, but found `{}`",
                    unexp_tok
                );

                let span = unexp_span;
                Err(ParseError {
                    src: self.src.clone(),
                    span,
                    message,
                    ctx: "parse a value literal".to_string(),
                })
            }
        }
    }

    fn parse_expr_lvalue(&mut self) -> Result<Spanned<Expr<()>>, ParseError> {
        let (lhs, lhs_span) = self.parse_lhs()?;
        Ok((Expr::Lvalue(Box::new(lhs), ()), lhs_span))
    }

    pub fn parse_expr_index(&mut self) -> Result<Spanned<Expr<()>>, ParseError> {
        let parse_index = |s: &mut Self| match s.expect(
            Token::Nat(0),
            "parse an index -- a natural number".to_string(),
        )? {
            (Token::Nat(n), sp) => Ok((n, sp)),
            _ => panic!(),
        };

        let bops: Vec<(
            Token,
            fn(
                Box<(Expr<()>, std::ops::Range<usize>)>,
                Box<(isize, std::ops::Range<usize>)>,
            ) -> Expr<()>,
        )> = vec![(
            Token::Dot,
            (|l, i| Expr::Index(l, (*i).0, ()))
                as fn(Box<Spanned<Expr<()>>>, Box<(isize, std::ops::Range<usize>)>) -> Expr<()>,
        )];
        self.macroparse_operator_binary_infix_left_assoc(
            bops,
            Self::parse_expr_atom,
            parse_index,
            "parse an expression_index".to_string(),
            self.left_span.clone(),
        )
    }

    fn parse_expr_immut_ref(&mut self) -> Result<Spanned<Expr<()>>, ParseError> {
        self.expect(Token::Amp, "parse a shared reference".to_string())?;

        let (name, sp) = match self.expect(
            Token::Var(String::new()),
            "parse an variable name".to_string(),
        )? {
            (Token::Var(s), sp) => (s, sp),
            _ => panic!(),
        };

        Ok((Expr::ImmutRef(name, ()), sp))
    }

    pub fn parse_expr_mut_ref(&mut self) -> Result<Spanned<Expr<()>>, ParseError> {
        self.expect_plur(
            vec![Token::Amp, Token::Mut],
            "parse a mutable reference".to_string(),
        )?;

        let (name, sp) = match self.expect(
            Token::Var(String::new()),
            "parse an variable name".to_string(),
        )? {
            (Token::Var(s), sp) => (s, sp),
            _ => panic!(),
        };

        Ok((Expr::MutRef(name, ()), sp))
    }

    pub fn parse_kexpr_alloc(&mut self) -> Result<Spanned<KExpr<()>>, ParseError> {
        let (_, spalloc) = self.expect(Token::Alloc, "parse an alloc".to_string())?;

        let (spanned_e, spe) =
            self.macroparse_wrap_delimiters(Self::parse_expr, Token::LParen, Token::RParen)?;

        let span = spalloc.start..spe.end;
        Ok((KExpr::Alloc(Box::new(spanned_e), ()), span))
    }

    pub fn parse_kexpr_free(&mut self) -> Result<Spanned<KExpr<()>>, ParseError> {
        let (_, spalloc) = self.expect(Token::Free, "parse an free".to_string())?;

        let (spanned_lhs, splhs) =
            self.macroparse_wrap_delimiters(Self::parse_lhs, Token::LParen, Token::RParen)?;

        let span = spalloc.start..splhs.end;
        Ok((KExpr::Free(Box::new(spanned_lhs), ()), span))
    }

    pub fn parse_expr_atom(&mut self) -> Result<Spanned<Expr<()>>, ParseError> {
        if self.lookahead_match_tokens(&vec![Token::LParen]) {
            if self.lookahead_match_tokens(&vec![Token::LParen, Token::RParen]) {
                // unit
                self.parse_expr_val()
            } else {
                let ((e, _e_span), delim_e_span) = self.macroparse_wrap_delimiters(
                    Self::parse_expr,
                    Token::LParen,
                    Token::RParen,
                )?;

                Ok((e, delim_e_span))
            }
        } else if self.lookahead_match_tokens(&vec![Token::Amp, Token::Mut]) {
            self.parse_expr_mut_ref()
        } else if self.lookahead_match_tokens(&vec![Token::Amp]) {
            self.parse_expr_immut_ref()
        } else if self.lookahead_match_tokens(&vec![
            Token::Var(String::new()),
            Token::DColon,
            Token::Var(String::new()),
            Token::LParen,
        ]) {
            self.parse_expr_bound_call()
        } else if self.lookahead_match_tokens(&vec![Token::Var(String::new()), Token::LParen]) {
            self.parse_expr_call()
        } else {
            self.metaparse_or(
                vec![Self::parse_expr_val, Self::parse_expr_lvalue],
                "parse a free, an alloc, a borrow, a call, value literal, or a path,".to_string(),
            )
        }
    }

    pub fn parse_expr_bang(&mut self) -> Result<Spanned<Expr<()>>, ParseError> {
        self.macroparse_operator_unary_prefix_hom(
            vec![(Token::Bang, |e| Expr::Bang(e, ()))],
            Self::parse_expr_index,
            "parse a bang boolean expression".to_string(),
            self.left_span.clone(),
        )
    }

    pub fn parse_expr_and(&mut self) -> Result<Spanned<Expr<()>>, ParseError> {
        self.macroparse_operator_binary_infix_left_assoc(
            vec![(Token::And, |e1, e2| Expr::And(e1, e2, ()))],
            Self::parse_expr_bang,
            Self::parse_expr_bang,
            "parse a conjunction boolean expression".to_string(),
            self.left_span.clone(),
        )
    }

    pub fn parse_expr_or(&mut self) -> Result<Spanned<Expr<()>>, ParseError> {
        self.macroparse_operator_binary_infix_left_assoc(
            vec![(Token::Or, |e1, e2| Expr::Or(e1, e2, ()))],
            Self::parse_expr_and,
            Self::parse_expr_and,
            "parse a disjunction boolean expression".to_string(),
            self.left_span.clone(),
        )
    }

    pub fn parse_expr_uminus(&mut self) -> Result<Spanned<Expr<()>>, ParseError> {
        self.macroparse_operator_unary_prefix_hom(
            vec![(Token::Minus, |e| Expr::Neg(e, ()))],
            Self::parse_expr_or,
            "parse a uminus arithmetic expression".to_string(),
            self.left_span.clone(),
        )
    }

    pub fn parse_expr_mult_div(&mut self) -> Result<Spanned<Expr<()>>, ParseError> {
        self.macroparse_operator_binary_infix_left_assoc(
            vec![
                (Token::Star, |e1, e2| Expr::Mult(e1, e2, ())),
                (Token::Divide, |e1, e2| Expr::Div(e1, e2, ())),
            ],
            Self::parse_expr_uminus,
            Self::parse_expr_uminus,
            "parse a product/quotient arithmetic expression".to_string(),
            self.left_span.clone(),
        )
    }

    pub fn parse_expr_add_sub(&mut self) -> Result<Spanned<Expr<()>>, ParseError> {
        self.macroparse_operator_binary_infix_left_assoc(
            vec![
                (Token::Plus, |e1, e2| Expr::Plus(e1, e2, ())),
                (Token::Minus, |e1, e2| Expr::Minus(e1, e2, ())),
            ],
            Self::parse_expr_mult_div,
            Self::parse_expr_mult_div,
            "parse a sum/difference arithmetic expression".to_string(),
            self.left_span.clone(),
        )
    }

    pub fn parse_expr_cmp(&mut self) -> Result<Spanned<Expr<()>>, ParseError> {
        self.macroparse_operator_binary_infix_left_assoc(
            vec![
                (Token::LessThan, |e1, e2| Expr::Lt(e1, e2, ())),
                (Token::CmpEq, |e1, e2| Expr::Eq(e1, e2, ())),
            ],
            Self::parse_expr_add_sub,
            Self::parse_expr_add_sub,
            "parse a comparison boolean expression".to_string(),
            self.left_span.clone(),
        )
    }

    pub fn parse_expr_bound_call(&mut self) -> Result<Spanned<Expr<()>>, ParseError> {
        let (type_name, type_name_span) =
            match self.expect(Token::Var(String::new()), "parse a type name".to_string())? {
                (Token::Var(s), sp) => (s, sp),
                _ => panic!(),
            };
        self.expect(Token::DColon, "parse method binding operator".to_string())?;

        let (fxn_name, _) = match self.expect(
            Token::Var(String::new()),
            "parse a function name".to_string(),
        )? {
            (Token::Var(s), sp) => (s, sp),
            _ => panic!(),
        };

        fn args(p: &mut Parser) -> Result<Vec<Spanned<Expr<()>>>, ParseError> {
            p.metaparse_separated_by(Parser::parse_expr, Token::Comma, false, 0)
        }

        let (v, v_span) = self.macroparse_wrap_delimiters(args, Token::LParen, Token::RParen)?;

        let span = type_name_span.start..v_span.end;

        Ok((Expr::BoundCall(type_name, fxn_name, v, ()), span))
    }
    pub fn parse_expr_call(&mut self) -> Result<Spanned<Expr<()>>, ParseError> {
        let (fxn_name, sp) = match self.expect(
            Token::Var(String::new()),
            "parse a function name".to_string(),
        )? {
            (Token::Var(s), sp) => (s, sp),
            _ => panic!(),
        };

        fn args(p: &mut Parser) -> Result<Vec<Spanned<Expr<()>>>, ParseError> {
            p.metaparse_separated_by(Parser::parse_expr, Token::Comma, false, 0)
        }

        let (v, v_span) = self.macroparse_wrap_delimiters(args, Token::LParen, Token::RParen)?;
        let span = sp.start..v_span.end;

        Ok((Expr::Call(fxn_name, v, ()), span))
    }
    pub fn parse_expr_tuple(&mut self) -> Result<Spanned<Expr<()>>, ParseError> {
        fn elements(p: &mut Parser) -> Result<Vec<Spanned<Expr<()>>>, ParseError> {
            p.metaparse_separated_by(Parser::parse_expr, Token::Comma, true, 1)
        }

        let (v, v_span) =
            self.macroparse_wrap_delimiters(elements, Token::LParen, Token::RParen)?;

        Ok((Expr::Tuple(v, ()), v_span))
    }

    pub fn parse_expr(&mut self) -> Result<Spanned<Expr<()>>, ParseError> {
        self.metaparse_or(
            vec![Self::parse_expr_cmp, Self::parse_expr_tuple],
            "parse expression".to_string(),
        )
    }

    pub fn parse_kexpr(&mut self) -> Result<Spanned<KExpr<()>>, ParseError> {
        if self.lookahead_match_tokens(&vec![Token::Free]) {
            self.parse_kexpr_free()
        } else if self.lookahead_match_tokens(&vec![Token::Alloc]) {
            self.parse_kexpr_alloc()
        } else {
            let (e, e_span) = self.parse_expr()?;
            Ok((KExpr::Expr(Box::new((e, e_span.clone()))), e_span))
        }
    }

    // parses all types but Type::Prod
    pub fn parse_type_atom(&mut self) -> Result<Spanned<Type>, ParseError> {
        match self.next("parse a primitive type or a custom type".to_string())? {
            (Token::TBool, sp) => Ok((Type::Bool, sp)),
            (Token::TInt, sp) => Ok((Type::Int, sp)),
            (Token::TUnit, sp) => Ok((Type::Unit, sp)),
            (Token::TRef, sp) => {
                let ((inner_tau, sp_inner_tau), delim_span) = self.macroparse_wrap_delimiters(
                    Self::parse_type,
                    Token::LessThan,
                    Token::GreaterThan,
                )?;

                Ok((
                    Type::Ref(Box::new((inner_tau, sp_inner_tau.clone()))),
                    sp.start..delim_span.end,
                ))
            }
            (Token::TRefMut, sp) => {
                let ((inner_tau, sp_inner_tau), delim_span) = self.macroparse_wrap_delimiters(
                    Self::parse_type,
                    Token::LessThan,
                    Token::GreaterThan,
                )?;

                Ok((
                    Type::RefMut(Box::new((inner_tau, sp_inner_tau.clone()))),
                    sp.start..delim_span.end,
                ))
            }
            (Token::TLoc, sp) => {
                let ((inner_tau, sp_inner_tau), delim_span) = self.macroparse_wrap_delimiters(
                    Self::parse_type,
                    Token::LessThan,
                    Token::GreaterThan,
                )?;

                Ok((
                    Type::Loc(Box::new((inner_tau, sp_inner_tau.clone()))),
                    sp.start..delim_span.end,
                ))
            }
            (Token::Var(s), sp) => Ok((Type::CustomType(s), sp)),
            (unexpected_tok, span) => Err(ParseError {
                span,
                message: format!(
                    "expected a primitive type, a custom type, or `(`, but found {}",
                    unexpected_tok
                ),
                src: self.src.clone(),
                ctx: "parse a primitive type or a custom type".to_string(),
            }),
        }
    }
    pub fn parse_type(&mut self) -> Result<Spanned<Type>, ParseError> {
        if self.lookahead_match_tokens(&vec![Token::LParen]) {
            fn elements(p: &mut Parser) -> Result<Vec<Spanned<Type>>, ParseError> {
                p.metaparse_separated_by(Parser::parse_type, Token::Comma, true, 1)
            }
            let (v, v_span) =
                self.macroparse_wrap_delimiters(elements, Token::LParen, Token::RParen)?;
            Ok((Type::Prod(v), v_span))
        } else {
            self.parse_type_atom()
        }
    }

    pub fn parse_heap_pred(&mut self) -> Result<Spanned<HeapPred>, ParseError> {
        let (_, hemp_span) = self.expect(Token::Hemp, "parse a heap_pred".to_string())?;
        Ok((HeapPred::Emp, hemp_span))
    }

    pub fn parse_pre_cond(&mut self) -> Result<Spanned<HeapPre>, ParseError> {
        if self.lookahead_match_tokens(&vec![Token::Pre]) {
            let (_, pre_span) = self.expect(Token::Pre, "parse a heap precondition".to_string())?;
            let (hp_pred, hp_pred_span) = self.parse_heap_pred()?;
            Ok((
                HeapPre::Pre(Box::new(hp_pred)),
                pre_span.start..hp_pred_span.end,
            ))
        } else {
            Ok((HeapPre::Vacuous, self.left_span.start..self.left_span.start))
        }
    }

    pub fn parse_post_cond(&mut self) -> Result<Spanned<HeapPost>, ParseError> {
        if self.lookahead_match_tokens(&vec![Token::Post]) {
            let (_, post_span) =
                self.expect(Token::Post, "parse a heap postcondition".to_string())?;
            let (hp_pred, hp_pred_span) = self.parse_heap_pred()?;
            Ok((
                HeapPost::Post(Box::new(hp_pred)),
                post_span.start..hp_pred_span.end,
            ))
        } else {
            Ok((
                HeapPost::Vacuous,
                self.left_span.start..self.left_span.start,
            ))
        }
    }

    pub fn parse_typed_arg(&mut self) -> Result<Spanned<(String, Type)>, ParseError> {
        let (name, name_span) = match self.expect(
            Token::Var(String::new()),
            "parse a function's name".to_string(),
        )? {
            (Token::Var(name), span) => (name, span),
            _ => panic!(),
        };
        self.expect(
            Token::Colon,
            "parse a type annotated argument name".to_string(),
        )?;
        let (tau, tau_span) = self.parse_type()?;
        Ok(((name, tau), name_span.start..tau_span.end))
    }

    pub fn parse_function(&mut self) -> Result<Spanned<FnDef<()>>, ParseError> {
        // n x(x1 : τ1, . . . , xi : τn) : τ {c; ; return e}
        let (_, fn_span) = self.expect(Token::FnDef, "parse a function definition".to_string())?;
        let spanned_name = match self.expect(
            Token::Var(String::new()),
            "parse a function definition".to_string(),
        )? {
            (Token::Var(name), span) => (name, span),
            _ => panic!(),
        };

        fn args(p: &mut Parser) -> Result<Vec<Spanned<(String, Type)>>, ParseError> {
            p.metaparse_separated_by(Parser::parse_typed_arg, Token::Comma, false, 0)
        }
        let (spanned_typed_args, _delim_ty_args_span) =
            self.macroparse_wrap_delimiters(args, Token::LParen, Token::RParen)?;

        self.expect(Token::Colon, "parse a function definition".to_string())?;

        let spanned_ret_ty = self.parse_type()?;

        fn body(
            p: &mut Parser,
        ) -> Result<Spanned<(Spanned<Cmd<()>>, Spanned<Expr<()>>)>, ParseError> {
            let (c, c_span) = p.parse_command()?;
            p.expect(
                Token::Return,
                "parse the return in a function definition".to_string(),
            )?;
            let (e, e_span) = p.parse_expr()?;
            Ok((
                ((c, c_span.clone()), (e, e_span.clone())),
                c_span.start..e_span.end,
            ))
        }
        let (((spanned_c, spanned_e), _body_inside_span), body_span) =
            self.macroparse_wrap_delimiters(body, Token::LCurBra, Token::RCurBra)?;

        Ok((
            FnDef(
                spanned_name,
                spanned_typed_args,
                Box::new(spanned_ret_ty),
                Box::new(spanned_c),
                Box::new(spanned_e),
            ),
            fn_span.start..body_span.end,
        ))
    }

    pub fn parse_impl_function(&mut self) -> Result<Spanned<ImplFnDef<()>>, ParseError> {
        let (_, fn_span) = self.expect(Token::FnDef, "parse a method definition".to_string())?;
        let spanned_name = match self.expect(
            Token::Var(String::new()),
            "parse a method definition".to_string(),
        )? {
            (Token::Var(name), span) => (name, span),
            _ => panic!(),
        };

        fn args(p: &mut Parser) -> Result<Vec<Spanned<(String, Type)>>, ParseError> {
            p.metaparse_separated_by(Parser::parse_typed_arg, Token::Comma, false, 0)
        }
        let (spanned_typed_args, _delim_ty_args_span) =
            self.macroparse_wrap_delimiters(args, Token::LParen, Token::RParen)?;

        self.expect(Token::Colon, "parse a method definition".to_string())?;

        let spanned_ret_ty = self.parse_type()?;

        let (spanned_heap_precond, _) =
            self.macroparse_wrap_delimiters(Self::parse_pre_cond, Token::LSqBra, Token::RSqBra)?;

        let (spanned_heap_postcond, _) =
            self.macroparse_wrap_delimiters(Self::parse_post_cond, Token::LSqBra, Token::RSqBra)?;

        fn body(
            p: &mut Parser,
        ) -> Result<Spanned<(Spanned<KCmd<()>>, Spanned<Expr<()>>)>, ParseError> {
            let (k, k_span) = p.parse_kcommand()?;
            p.expect(
                Token::Return,
                "parse the return in a method definition".to_string(),
            )?;
            let (e, e_span) = p.parse_expr()?;
            Ok((
                ((k, k_span.clone()), (e, e_span.clone())),
                k_span.start..e_span.end,
            ))
        }
        let (((spanned_k, spanned_e), _body_inside_span), body_span) =
            self.macroparse_wrap_delimiters(body, Token::LCurBra, Token::RCurBra)?;
        let span = fn_span.start..body_span.end;
        Ok((
            ImplFnDef(
                spanned_name,
                spanned_typed_args,
                Box::new(spanned_ret_ty),
                Box::new(spanned_heap_precond),
                Box::new(spanned_heap_postcond),
                Box::new(spanned_k),
                Box::new(spanned_e),
            ),
            span,
        ))
    }

    fn parse_decl(&mut self) -> Result<Spanned<Decl<()>>, ParseError> {
        if self.lookahead_match_tokens(&vec![Token::TypeDef]) {
            let (_, decl_span) =
                self.expect(Token::TypeDef, "parse a type definition".to_string())?;

            let spanned_name = match self.expect(
                Token::Var(String::new()),
                "parse a type definition".to_string(),
            )? {
                (Token::Var(name), span) => (name, span),
                _ => panic!(),
            };

            fn types(p: &mut Parser) -> Result<Vec<Spanned<Type>>, ParseError> {
                p.metaparse_separated_by(Parser::parse_type, Token::Comma, false, 0)
            }
            let (v, v_span) =
                self.macroparse_wrap_delimiters(types, Token::LParen, Token::RParen)?;
            let span = decl_span.start..v_span.end;
            Ok((Decl::TypeDef(spanned_name, v), span))
        } else if self.lookahead_match_tokens(&vec![Token::Impl]) {
            let (_, impl_span) =
                self.expect(Token::Impl, "parse a type implementation".to_string())?;

            let spanned_name = match self.expect(
                Token::Var(String::new()),
                "parse a type definition".to_string(),
            )? {
                (Token::Var(name), span) => (name, span),
                _ => panic!(),
            };

            let (spanned_heap_pred, _) = self.macroparse_wrap_delimiters(
                Self::parse_heap_pred,
                Token::LSqBra,
                Token::RSqBra,
            )?;

            fn implfxns(p: &mut Parser) -> Result<Vec<Spanned<ImplFnDef<()>>>, ParseError> {
                p.metaparse_separated_by(Parser::parse_impl_function, Token::Comma, true, 0)
            }
            let (methods, m_span) =
                self.macroparse_wrap_delimiters(implfxns, Token::LCurBra, Token::RCurBra)?;

            let span = impl_span.start..m_span.end;
            Ok((
                Decl::TypeImpl(spanned_name, spanned_heap_pred, methods),
                span,
            ))
        } else {
            let (unexpected_tok, tok_span) = self.next("parse a declaration".to_string())?;
            Err(ParseError {
                span: tok_span,
                message: format!(
                    "expected `{}` or `{}`, but found `{}`",
                    Token::TypeDef,
                    Token::Impl,
                    unexpected_tok
                ),
                src: self.src.clone(),
                ctx: "parse a declaration".to_string(),
            })
        }
    }

    pub fn parse_command_decl(&mut self) -> Result<Spanned<Cmd<()>>, ParseError> {
        let (decl, decl_span) = self.parse_decl()?;
        Ok((
            Cmd::TypeDecl(Box::new((decl, decl_span.clone()))),
            decl_span,
        ))
    }

    pub fn parse_command_fn(&mut self) -> Result<Spanned<Cmd<()>>, ParseError> {
        let (fn_def, fn_span) = self.parse_function()?;
        Ok((Cmd::FxnDefin(Box::new((fn_def, fn_span.clone()))), fn_span))
    }

    pub fn parse_generic_while<T>(
        &mut self,
        while_disc: fn(Box<Spanned<Expr<()>>>, Box<Spanned<T>>) -> T,
        body_parser: fn(&mut Parser) -> Result<Spanned<T>, ParseError>,
        grammar_part: String,
    ) -> Result<Spanned<T>, ParseError> {
        let (_, while_span) = self.expect(
            Token::While,
            format!("parse a while {}", grammar_part.clone()),
        )?;
        let spanned_cond = self.parse_expr()?;

        if !self.lookahead_match_tokens(&vec![Token::LCurBra]) {
            let (unexpected_tok, span) =
                self.next(format!("parse a while {}'s body", grammar_part.clone()))?;
            Err(ParseError {
                span,
                message: format!(
                    "expected `{}`, but found `{}`",
                    Token::LCurBra,
                    unexpected_tok
                ),
                src: self.src.clone(),
                ctx: format!("parse a while {}'s body", grammar_part),
            })?;
        }
        let ((body, body_inside_span), body_span) =
            self.macroparse_wrap_delimiters(body_parser, Token::LCurBra, Token::RCurBra)?;
        let span = while_span.start..body_span.end;
        Ok((
            while_disc(Box::new(spanned_cond), Box::new((body, body_inside_span))),
            span,
        ))
    }
    pub fn parse_generic_if<T>(
        &mut self,
        if_disc: fn(Box<Spanned<Expr<()>>>, Box<Spanned<T>>, Box<Spanned<T>>) -> T,
        body_parser: fn(&mut Parser) -> Result<Spanned<T>, ParseError>,
        grammar_part: String,
    ) -> Result<Spanned<T>, ParseError> {
        let (_, if_span) =
            self.expect(Token::If, format!("parse an if {}", grammar_part.clone()))?;

        let spanned_cond = self.parse_expr()?;

        if !self.lookahead_match_tokens(&vec![Token::LCurBra]) {
            let (unexpected_tok, span) = self.next(format!(
                "parse an if {}'s then branch",
                grammar_part.clone()
            ))?;
            Err(ParseError {
                span,
                message: format!(
                    "expected `{}`, but found `{}`",
                    Token::LCurBra,
                    unexpected_tok
                ),
                src: self.src.clone(),
                ctx: format!("parse an if {}'s then branch", grammar_part.clone()),
            })?;
        }
        let ((then_body, then_body_span), _) =
            self.macroparse_wrap_delimiters(body_parser, Token::LCurBra, Token::RCurBra)?;

        self.expect(
            Token::Else,
            format!("parse an if {}'s else branch", grammar_part.clone()),
        )?;

        if !self.lookahead_match_tokens(&vec![Token::LCurBra]) {
            let (unexpected_tok, span) = self.next(format!(
                "parse an if {}'s else branch",
                grammar_part.clone()
            ))?;
            Err(ParseError {
                span,
                message: format!(
                    "expected `{}`, but found `{}`",
                    Token::LCurBra,
                    unexpected_tok
                ),
                src: self.src.clone(),
                ctx: format!("parse an if {}'s else branch", grammar_part.clone()),
            })?;
        }
        let ((else_body, else_body_span), else_delim_body_span) =
            self.macroparse_wrap_delimiters(body_parser, Token::LCurBra, Token::RCurBra)?;

        let span = if_span.start..else_delim_body_span.end;
        Ok((
            if_disc(
                Box::new(spanned_cond),
                Box::new((then_body, then_body_span.clone())),
                Box::new((else_body, else_body_span.clone())),
            ),
            span,
        ))
    }

    pub fn parse_generic_let<S, T>(
        &mut self,
        let_disc: fn(Spanned<String>, Box<Spanned<Type>>, Box<Spanned<S>>) -> T,
        rhs_parser: fn(&mut Parser) -> Result<Spanned<S>, ParseError>,
        grammar_part: String,
    ) -> Result<Spanned<T>, ParseError> {
        let (_, let_span) = self.expect(
            Token::Let,
            format!("parse a let assignment {}", grammar_part),
        )?;

        let spanned_name = match self.expect(
            Token::Var(String::new()),
            format!("parse a let assignment {}'s name", grammar_part),
        )? {
            (Token::Var(name), span) => (name, span),
            _ => panic!(),
        };

        self.expect(
            Token::Colon,
            format!("parse a let assignment {}'s  type annotation", grammar_part),
        )?;

        let spanned_ty = self.parse_type()?;

        self.expect(
            Token::Equals,
            format!("parse a let assignment {}", grammar_part),
        )?;

        let (e, e_span) = rhs_parser(self)?;

        Ok((
            let_disc(
                spanned_name,
                Box::new(spanned_ty),
                Box::new((e, e_span.clone())),
            ),
            let_span.start..e_span.end,
        ))
    }

    pub fn parse_generic_let_mut<S, T>(
        &mut self,
        let_disc: fn(Spanned<String>, Box<Spanned<Type>>, Box<Spanned<S>>) -> T,
        rhs_parser: fn(&mut Parser) -> Result<Spanned<S>, ParseError>,
        grammar_part: String,
    ) -> Result<Spanned<T>, ParseError> {
        let (_, let_span) = self.expect(
            Token::Let,
            format!("parse a let-mut assignment {}", grammar_part),
        )?;

        self.expect(
            Token::Mut,
            format!("parse a let-mut assignment {}", grammar_part),
        )?;

        let spanned_name = match self.expect(
            Token::Var(String::new()),
            format!("parse a let-mut assignment {}'s name", grammar_part),
        )? {
            (Token::Var(name), span) => (name, span),
            _ => panic!(),
        };

        self.expect(
            Token::Colon,
            format!(
                "parse a let-mut assignment {}'s  type annotation",
                grammar_part
            ),
        )?;

        let spanned_ty = self.parse_type()?;

        self.expect(
            Token::Equals,
            format!("parse a let-mut assignment {}", grammar_part),
        )?;

        let (e, e_span) = rhs_parser(self)?;

        Ok((
            let_disc(
                spanned_name,
                Box::new(spanned_ty),
                Box::new((e, e_span.clone())),
            ),
            let_span.start..e_span.end,
        ))
    }

    pub fn parse_generic_assign<S, T>(
        &mut self,
        assign_disc: fn(Box<Spanned<Lhs<()>>>, Box<Spanned<S>>) -> T,
        rhs_parser: fn(&mut Parser) -> Result<Spanned<S>, ParseError>,
        grammar_part: String,
    ) -> Result<Spanned<T>, ParseError> {
        let (lhs, lhs_span) = self.parse_lhs()?;
        self.expect(Token::Equals, format!("parse an assign {grammar_part}"))?;
        let (e, e_span) = rhs_parser(self)?;
        Ok((
            assign_disc(
                Box::new((lhs, lhs_span.clone())),
                Box::new((e, e_span.clone())),
            ),
            lhs_span.start..e_span.end,
        ))
    }

    pub fn parse_generic_scope<T>(
        &mut self,
        scope_disc: fn(Box<Spanned<T>>) -> T,
        body_parser: fn(&mut Parser) -> Result<Spanned<T>, ParseError>,
    ) -> Result<Spanned<T>, ParseError> {
        let (spanned_c, sc_span) =
            self.macroparse_wrap_delimiters(body_parser, Token::LCurBra, Token::RCurBra)?;

        Ok((scope_disc(Box::new(spanned_c)), sc_span))
    }

    pub fn parse_command_atom(&mut self) -> Result<Spanned<Cmd<()>>, ParseError> {
        if self.lookahead_match_tokens(&vec![Token::LCurBra]) {
            self.parse_generic_scope(Cmd::Scope, |p| p.parse_command())
        } else if self.lookahead_match_tokens(&vec![Token::TypeDef])
            || self.lookahead_match_tokens(&vec![Token::Impl])
        {
            self.parse_command_decl()
        } else if self.lookahead_match_tokens(&vec![Token::While]) {
            self.parse_generic_while(Cmd::While, |p| p.parse_command(), "command".to_string())
        } else if self.lookahead_match_tokens(&vec![Token::FnDef]) {
            self.parse_command_fn()
        } else if self.lookahead_match_tokens(&vec![Token::If]) {
            self.parse_generic_if(Cmd::If, |p| p.parse_command(), "command".to_string())
        } else if self.lookahead_match_tokens(&vec![Token::Let]) {
            if self.lookahead_match_tokens(&vec![Token::Let, Token::Mut]) {
                self.parse_generic_let_mut(Cmd::LetMut, |p| p.parse_expr(), "command".to_string())
            } else {
                self.parse_generic_let(Cmd::Let, |p| p.parse_expr(), "command".to_string())
            }
        } else if self.lookahead_is_lhs() {
            self.parse_generic_assign(Cmd::Assign, |p| p.parse_expr(), "command".to_string())
        } else {
            let (_, sk_span) =
                self.expect(Token::Skip, "parse a non-sequence command".to_string())?;
            Ok((Cmd::Skip, sk_span))
        }
    }

    pub fn parse_command(&mut self) -> Result<Spanned<Cmd<()>>, ParseError> {
        self.macroparse_operator_binary_infix_left_assoc(
            vec![(Token::Semicolon, |c1, c2| Cmd::Sequence(c1, c2))],
            Self::parse_command_atom,
            Self::parse_command_atom,
            "parse a sequence of commands".to_string(),
            self.left_span.clone(),
        )
    }

    fn parse_kcommand_lemma(&mut self) -> Result<Spanned<KCmd<()>>, ParseError> {
        let (_, lemma_span) = self.expect(Token::Lemma, "parse a lemma".to_string())?;
        let (hp, hp_span) = self.parse_heap_pred()?;
        let span = lemma_span.start..hp_span.end;
        Ok((KCmd::Lemma(Box::new((hp, hp_span))), span))
    }

    pub fn parse_kcommand_atom(&mut self) -> Result<Spanned<KCmd<()>>, ParseError> {
        if self.lookahead_match_tokens(&vec![Token::LCurBra]) {
            self.parse_generic_scope(KCmd::Scope, |p| p.parse_kcommand())
        } else if self.lookahead_match_tokens(&vec![Token::While]) {
            self.parse_generic_while(KCmd::While, |p| p.parse_kcommand(), "k-command".to_string())
        } else if self.lookahead_match_tokens(&vec![Token::If]) {
            self.parse_generic_if(KCmd::If, |p| p.parse_kcommand(), "k-command".to_string())
        } else if self.lookahead_match_tokens(&vec![Token::Let]) {
            if self.lookahead_match_tokens(&vec![Token::Let, Token::Mut]) {
                self.parse_generic_let_mut(
                    KCmd::LetMut,
                    |p| p.parse_kexpr(),
                    "k-command".to_string(),
                )
            } else {
                self.parse_generic_let(KCmd::Let, |p| p.parse_kexpr(), "k-command".to_string())
            }
        } else if self.lookahead_match_tokens(&vec![Token::Lemma]) {
            self.parse_kcommand_lemma()
        } else if self.lookahead_is_lhs() {
            self.parse_generic_assign(KCmd::Assign, |p| p.parse_kexpr(), "k-command".to_string())
        } else {
            let (_, sk_span) =
                self.expect(Token::Skip, "parse a non-sequence k-command".to_string())?;
            Ok((KCmd::Skip, sk_span))
        }
    }

    pub fn parse_kcommand(&mut self) -> Result<Spanned<KCmd<()>>, ParseError> {
        self.macroparse_operator_binary_infix_left_assoc(
            vec![(Token::Semicolon, |c1, c2| KCmd::Sequence(c1, c2))],
            Self::parse_kcommand_atom,
            Self::parse_kcommand_atom,
            "parse a sequence of k-commands".to_string(),
            self.left_span.clone(),
        )
    }
}
