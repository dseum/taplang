use std::collections::VecDeque;

use crate::ast::{
    Cmd, Decl, Expr, FnDef, HeapPost, HeapPre, HeapPred, ImplFnDef, Lhs, Span, Spanned, Type, Value,
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

    // fn metaparse_bin_or<T>(
    //     &mut self,
    //     f1: fn(&mut Self) -> Result<Spanned<T>, ParseError>,
    //     f2: fn(&mut Self) -> Result<Spanned<T>, ParseError>,
    //     ctx: String,
    // ) -> Result<Spanned<T>, ParseError> {
    //     let tokens_cop = self.tokens.clone();
    //     match f1(self) {
    //         Ok(spanned_expr1) => Ok(spanned_expr1),
    //         Err(e1) => match f2(self) {
    //             Ok(spanned_expr2) => {
    //                 self.tokens.clear();
    //                 self.tokens.extend(tokens_cop);
    //                 Ok(spanned_expr2)
    //             }
    //             Err(e2) => {
    //                 Err(ParseError {
    //                     src: self.src.clone(),
    //                     span: (e1.span.start.min(e2.span.start).. e1.span.end.min( e2.span.end)),
    //                     message: format!("parse two possible grammar rules, but both failed. Here are their individual messages:
    //                 {}

    //                 {}", e1.to_string(), e2.to_string()),
    //                     ctx
    //                 })
    //             }
    //         },
    //     }
    // }

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
    ) -> Result<Spanned<T>, ParseError> {
        if self.lookahead_match_tokens(&vec![left_delimiter.clone()]) {
            self.macroparse_wrap_delimiters(parse, left_delimiter, right_delimiter)
        } else {
            parse(self)
        }
    }

    fn macroparse_wrap_delimiters<T>(
        &mut self,
        parse: fn(&mut Self) -> Result<T, ParseError>,
        left_delimiter: Token,
        right_delimiter: Token,
    ) -> Result<T, ParseError> {
        self.expect(
            left_delimiter.clone(),
            format!(
                "parse a {} {} delimted grammatical unit, but failed to open.",
                left_delimiter.clone(),
                right_delimiter.clone()
            ),
        )?;

        let spanned_inner = parse(self)?;

        self.expect(
            right_delimiter.clone(),
            format!(
                "parse a {} {} delimted grammatical unit, but failed to close.",
                left_delimiter, right_delimiter
            ),
        )?;
        Ok(spanned_inner)
    }

    fn parse_lhs_name(&mut self) -> Result<Spanned<Lhs<()>>, ParseError> {
        match self.expect(Token::Var(String::new()), "parse a name".to_string())? {
            (Token::Var(name), span) => Ok((Lhs::Var(name, ()), span)),
            _ => panic!(),
        }
    }

    fn parse_lhs_atom(&mut self) -> Result<Spanned<Lhs<()>>, ParseError> {
        if self.lookahead_match_tokens(&vec![Token::LParen]) {
            self.macroparse_wrap_delimiters(Self::parse_lhs, Token::LParen, Token::RParen)
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

    pub fn parse_expr_alloc(&mut self) -> Result<Spanned<Expr<()>>, ParseError> {
        let (_, spalloc) = self.expect(Token::Alloc, "parse an alloc".to_string())?;

        let (e, spe) =
            self.macroparse_wrap_delimiters(Self::parse_expr, Token::LParen, Token::RParen)?;

        let span = spalloc.start..spe.end;
        Ok((Expr::Alloc(Box::new((e, spe)), ()), span))
    }

    pub fn parse_expr_free(&mut self) -> Result<Spanned<Expr<()>>, ParseError> {
        let (_, spalloc) = self.expect(Token::Free, "parse an free".to_string())?;

        let (lhs, splhs) =
            self.macroparse_wrap_delimiters(Self::parse_lhs, Token::LParen, Token::RParen)?;

        let span = spalloc.start..splhs.end;
        Ok((Expr::Free(Box::new((lhs, splhs)), ()), span))
    }

    pub fn parse_expr_atom(&mut self) -> Result<Spanned<Expr<()>>, ParseError> {
        if self.lookahead_match_tokens(&vec![Token::LParen]) {
            if self.lookahead_match_tokens(&vec![Token::LParen, Token::RParen]) {
                // unit
                self.parse_expr_val()
            } else {
                self.macroparse_wrap_delimiters(Self::parse_expr, Token::LParen, Token::RParen)
            }
        } else if self.lookahead_match_tokens(&vec![Token::Free]) {
            self.parse_expr_free()
        } else if self.lookahead_match_tokens(&vec![Token::Alloc]) {
            self.parse_expr_alloc()
        } else if self.lookahead_match_tokens(&vec![Token::Amp, Token::Mut]) {
            self.parse_expr_mut_ref()
        } else if self.lookahead_match_tokens(&vec![Token::Amp]) {
            self.parse_expr_immut_ref()
        } else if self.lookahead_match_tokens(&vec![
            Token::Var(String::new()),
            Token::DScolon,
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
        let span_start = self.left_span.start;
        let (type_name, sp) =
            match self.expect(Token::Var(String::new()), "parse a type name".to_string())? {
                (Token::Var(s), sp) => (s, sp),
                _ => panic!(),
            };
        self.expect(Token::DColon, "parse method binding operator".to_string())?;

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

        let v = self.macroparse_wrap_delimiters(args, Token::LParen, Token::RParen)?;

        let span_end = self.left_span.start;

        Ok((
            Expr::BoundCall(type_name, fxn_name, v, ()),
            span_start..span_end,
        ))
    }
    pub fn parse_expr_call(&mut self) -> Result<Spanned<Expr<()>>, ParseError> {
        let span_start = self.left_span.start;

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

        let v = self.macroparse_wrap_delimiters(args, Token::LParen, Token::RParen)?;

        let span_end = self.left_span.start;

        Ok((Expr::Call(fxn_name, v, ()), span_start..span_end))
    }
    pub fn parse_expr_tuple(&mut self) -> Result<Spanned<Expr<()>>, ParseError> {
        let span_start = self.left_span.start;

        fn elements(p: &mut Parser) -> Result<Vec<Spanned<Expr<()>>>, ParseError> {
            p.metaparse_separated_by(Parser::parse_expr, Token::Comma, true, 1)
        }

        let v = self.macroparse_wrap_delimiters(elements, Token::LParen, Token::RParen)?;

        let span_end = self.left_span.start;

        Ok((Expr::Tuple(v, ()), span_start..span_end))
    }

    pub fn parse_expr(&mut self) -> Result<Spanned<Expr<()>>, ParseError> {
        self.metaparse_or(
            vec![Self::parse_expr_cmp, Self::parse_expr_tuple],
            "parse expression".to_string(),
        )
    }

    // parses all types but Type::Prod
    pub fn parse_type_atom(&mut self) -> Result<Spanned<Type>, ParseError> {
        match self.next("parse a primitive type or a custom type".to_string())? {
            (Token::TBool, sp) => Ok((Type::Bool, sp)),
            (Token::TInt, sp) => Ok((Type::Int, sp)),
            (Token::TUnit, sp) => Ok((Type::Unit, sp)),
            (Token::TRef, sp) => {
                let (inner_tau, sp_inner_tau) = self.macroparse_wrap_delimiters(
                    Self::parse_type,
                    Token::LessThan,
                    Token::GreaterThan,
                )?;

                Ok((
                    Type::Ref(Box::new((inner_tau, sp_inner_tau.clone()))),
                    sp.start..sp_inner_tau.end,
                ))
            }
            (Token::TRefMut, sp) => {
                let (inner_tau, sp_inner_tau) = self.macroparse_wrap_delimiters(
                    Self::parse_type,
                    Token::LessThan,
                    Token::GreaterThan,
                )?;

                Ok((
                    Type::RefMut(Box::new((inner_tau, sp_inner_tau.clone()))),
                    sp.start..sp_inner_tau.end,
                ))
            }
            (Token::TLoc, sp) => {
                let (inner_tau, sp_inner_tau) = self.macroparse_wrap_delimiters(
                    Self::parse_type,
                    Token::LessThan,
                    Token::GreaterThan,
                )?;

                Ok((
                    Type::Loc(Box::new((inner_tau, sp_inner_tau.clone()))),
                    sp.start..sp_inner_tau.end,
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
            let span_start = self.left_span.start;
            fn elements(p: &mut Parser) -> Result<Vec<Spanned<Type>>, ParseError> {
                p.metaparse_separated_by(Parser::parse_type, Token::Comma, true, 1)
            }
            let v = self.macroparse_wrap_delimiters(elements, Token::LParen, Token::RParen)?;
            let span_end = self.left_span.start;
            Ok((Type::Prod(v), span_start..span_end))
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
        let spanned_typed_args =
            self.macroparse_wrap_delimiters(args, Token::LParen, Token::RParen)?;

        self.expect(Token::Colon, "parse a function definition".to_string())?;

        let spanned_ret_ty = self.parse_type()?;

        fn body(
            p: &mut Parser,
        ) -> Result<Spanned<(Spanned<Cmd<()>>, Spanned<Expr<()>>)>, ParseError> {
            let (c, c_span) = p.parse_command()?;
            p.expect(
                Token::Semicolon,
                "parse the body of a function definition".to_string(),
            )?;
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
        let ((spanned_c, spanned_e), body_span) =
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

    pub fn parse_command(&mut self) -> Result<Spanned<Cmd<()>>, ParseError> {
        /*

        d | f | {c} | c1; c2 | while b{c} | if b {c1} else {c2} | skip
        | ℓ = e | let x : τ = e | let mut x : τ = e

        */

        if self.lookahead_match_tokens(&vec![Token::Skip]) {
            let (_, skip_span) = self.expect(Token::Skip, "parse a skip command".to_string())?;
            Ok((Cmd::Skip, skip_span))
        } else {
            Ok((Cmd::Skip, self.left_span.clone()))
        }
    }
}

// fn parse_expr_cond(tokens: &mut VecDeque<Spanned<Token>>) -> Result<Spanned<EExpr>, ParseError> {
//     let (if_tok, if_span) = consume(tokens)?;
//     match if_tok {
//         Token::If => Ok(()),
//         _ => Err(ParseError {msg: "While parsing a conditional expression, the parser expected an `if` token to be at the start".to_string(), span: if_span})
//     }?;

//     let spanned_e1 = parse_expr_cmp(tokens)?;

//     let (then_tok, then_span) = consume(tokens)?;
//     match then_tok {
//         Token::Then => Ok(()),
//         _ => Err(ParseError {
//             msg: "While parsing a conditional expression, the parser expected a `then` token"
//                 .to_string(),
//             span: then_span,
//         }),
//     }?;

//     let spanned_e2 = parse_expr_cmp(tokens)?;

//     let (else_tok, else_span) = consume(tokens)?;
//     match else_tok {
//         Token::Else => Ok(()),
//         _ => Err(ParseError {
//             msg: "While parsing a conditional expression, the parser expected an `else` token"
//                 .to_string(),
//             span: else_span,
//         }),
//     }?;

//     let spanned_e3 = parse_expr_cmp(tokens)?;

//     let span = (spanned_e1.1.start)..(spanned_e3.1.end);

//     Ok((
//         EExpr::Cond(
//             Box::new(spanned_e1),
//             Box::new(spanned_e2),
//             Box::new(spanned_e3),
//         ),
//         span,
//     ))
// }

// fn parse_expr_normal(tokens: &mut VecDeque<Spanned<Token>>) -> Result<Spanned<EExpr>, ParseError> {
//     if lookahead_match_tokens(tokens, &vec![Token::Op("[".to_string())]).0 {
//         parse_expr_tuple(tokens)
//     } else if lookahead_match_tokens(tokens, &vec![Token::If]).0 {
//         parse_expr_cond(tokens)
//     } else {
//         parse_expr_cmp(tokens)
//     }
// }

// fn parse_expr_assign(tokens: &mut VecDeque<Spanned<Token>>) -> Result<Spanned<EExpr>, ParseError> {
//     let targets: &mut Vec<Spanned<ELhs>> = &mut vec![];

//     while lookahead_match_tokens_conj(tokens, lookahead_is_lhs, |tokens| {
//         lookahead_match_tokens(tokens, &vec![Token::Op("=".to_string())])
//     })
//     .0
//     {
//         let spanned_lhs = parse_lhs(tokens)?;
//         let (eq, eq_span) = consume(tokens)?;
//         eq.expect(Token::Op("=".to_string()), eq_span)?;
//         targets.push(spanned_lhs);
//     }

//     let spanned_e = parse_expr_normal(tokens)?;
//     Ok(targets
//         .into_iter()
//         .rfold(spanned_e, |acc, (lhs, lhs_span)| {
//             let span = (lhs_span.start)..(acc.1.end);
//             (
//                 EExpr::Assign((lhs.to_owned(), lhs_span.to_owned()), Box::new(acc)),
//                 span,
//             )
//         }))
// }

// fn parse_expr_seq(tokens: &mut VecDeque<Spanned<Token>>) -> Result<Spanned<EExpr>, ParseError> {
//     macroparse_operator_binary_infix_hom(
//         tokens,
//         vec![(vec![Token::Op(";".to_string())], EExpr::Seq)],
//         |tokens| metaparse_or(tokens, parse_expr_assign, parse_expr_normal),
//     )
// }

// fn parse_ident(tokens: &mut VecDeque<Spanned<Token>>) -> Result<String, ParseError> {
//     match consume(tokens)? {
//         (Token::Var(ident), _) => Ok(ident),
//         (_, span) => Err(ParseError {
//             msg: "Expected identifier".to_string(),
//             span,
//         }),
//     }
// }

// fn parse_expr_let(tokens: &mut VecDeque<Spanned<Token>>) -> Result<Spanned<EExpr>, ParseError> {
//     let spanned_let = consume(tokens)?;
//     let start = spanned_let.1.start;
//     spanned_let.0.expect(Token::Let, spanned_let.1)?;

//     let name = parse_ident(tokens)?;

//     let spanned_eq = consume(tokens)?;
//     spanned_eq
//         .0
//         .expect(Token::Op("=".to_string()), spanned_eq.1)?;

//     let rhs = parse_expr_normal(tokens)?;

//     let spanned_in = consume(tokens)?;
//     spanned_in.0.expect(Token::In, spanned_in.1)?;

//     let then = parse_expr(tokens)?;

//     let span = (start)..(then.1.end);
//     Ok((
//         EExpr::Let {
//             name,
//             rhs: Box::new(rhs),
//             then: Box::new(then),
//         },
//         span,
//     ))
// }

// fn parse_expr_mutlet(tokens: &mut VecDeque<Spanned<Token>>) -> Result<Spanned<EExpr>, ParseError> {
//     let spanned_let = consume(tokens)?;
//     let start = spanned_let.1.start;
//     spanned_let.0.expect(Token::Let, spanned_let.1)?;
//     let spanned_mut = consume(tokens)?;
//     spanned_mut.0.expect(Token::Mut, spanned_mut.1)?;

//     let name = parse_ident(tokens)?;

//     let spanned_eq = consume(tokens)?;
//     spanned_eq
//         .0
//         .expect(Token::Op("=".to_string()), spanned_eq.1)?;

//     let rhs = parse_expr_normal(tokens)?;

//     let spanned_in = consume(tokens)?;
//     spanned_in.0.expect(Token::In, spanned_in.1)?;

//     let then = parse_expr(tokens)?;

//     let span = (start)..(then.1.end);
//     Ok((
//         EExpr::MutLet {
//             name,
//             rhs: Box::new(rhs),
//             then: Box::new(then),
//         },
//         span,
//     ))
// }

// pub fn parse_expr(tokens: &mut VecDeque<Spanned<Token>>) -> Result<Spanned<EExpr>, ParseError> {
//     if lookahead_match_tokens(tokens, &vec![Token::Let, Token::Mut]).0 {
//         parse_expr_mutlet(tokens)
//     } else if lookahead_match_tokens(tokens, &vec![Token::Let]).0 {
//         parse_expr_let(tokens)
//     } else {
//         parse_expr_seq(tokens)
//     }
// }

// pub fn expr_parser(tokens: &mut VecDeque<Spanned<Token>>) -> Result<Spanned<EExpr>, ParseError> {
//     let spanned_e = parse_expr(tokens)?;
//     if let Some(spanned_end) = tokens.pop_back() {
//         Err(ParseError {
//             msg: "Expected EOF".to_string(),
//             span: (spanned_e.1.end)..(spanned_end.1.end),
//         })
//     } else {
//         Ok(spanned_e)
//     }
// }
