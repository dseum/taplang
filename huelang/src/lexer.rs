use chumsky::prelude::*;

// use crate::ast::Spanned;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Nat(u64),
    Var(String),
    Let,
    Mut,
    Alloc,
    Free,
    If,
    Then,
    Else,
    While,
    Do,
    True,
    False,
    Plus,
    Minus,
    Mult,
    Div,
    Bang,
    And,
    Lt,
    Eq,
    Semicolon,
}

// impl Token {
//     pub fn expect(self, tok: Token, span: Span) -> Result<Self, ParseError> {
//         if self == tok {
//             Ok(self)
//         } else {
//             Err(ParseError {
//                 msg: format!("Expected {:?} but instead found {:?}", tok, self),
//                 span,
//             })
//         }
//     }

//     pub fn posh_expect(self, tok: Token, span: Span, message: String) -> Result<Self, ParseError> {
//         if self == tok {
//             Ok(self)
//         } else {
//             Err(ParseError { msg: message, span })
//         }
//     }
// }

// #[derive(Debug)]
// pub struct ParseError {
//     pub msg: String,
//     pub span: Span,
// }

// impl ParseError {
//     pub fn prettify(&self) -> String {
//         format!("At {:?}, ParseError was raised: {}", self.span, self.msg)
//     }
//     pub fn wrap(&self, burrito: Self) -> Self {
//         let innie: String = self
//             .prettify()
//             .split("\n")
//             .into_iter()
//             .map(|s| format!("\t{}", s))
//             .collect();
//         let msg = format!("{}\n{}", burrito.msg, innie);
//         ParseError {
//             msg,
//             span: burrito.span,
//         }
//     }
// }

#[derive(Debug, PartialEq)]
pub struct Spanned<T>(pub T, pub SimpleSpan<usize>);

#[allow(clippy::let_and_return)]
fn lex<'src>() -> impl Parser<'src, &'src str, Vec<Spanned<Token>>> + Clone {
    let nat = text::int(10).map(|n: &'src str| Token::Nat(n.parse().unwrap()));

    // let newline = just("\n").or(just("\r\n")).to(Token::Eol);
    let op = choice((
        just("==").to(Token::Eq),
        just("<").to(Token::Lt),
        just("+").to(Token::Plus),
        just("-").to(Token::Minus),
        just("*").to(Token::Mult),
        just("/").to(Token::Div),
        just("!").to(Token::Bang),
        just("&&").to(Token::And),
        just(";").to(Token::Semicolon),
    ));

    let keyword_or_ident = text::ident().map(|ident: &'src str| match ident {
        "let" => Token::Let,
        "mut" => Token::Mut,
        "if" => Token::If,
        "then" => Token::Then,
        "else" => Token::Else,
        "true" => Token::True,
        "false" => Token::False,
        "alloc" => Token::Alloc,
        "free" => Token::Free,
        "while" => Token::While,
        "do" => Token::Do,
        _ => Token::Var(ident.to_string()),
    });
    let whitespace = just(" ").or(just("\t")).or(just("\n").or(just("\r\n")));

    choice((nat, op, keyword_or_ident))
        .map_with(|tok, e| Spanned(tok, e.span()))
        // Skip optional whitespace around tokens
        .padded_by(whitespace)
        // Parse zero or more tokens
        .repeated()
        .collect()
        // End of input
        .then_ignore(end())
}
