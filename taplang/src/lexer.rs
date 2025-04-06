use logos::Logos;

/* ANCHOR: tokens */
#[derive(Logos, Debug, PartialEq, Eq, Hash, Clone)]
#[logos(error = String)]
pub enum Token {
    #[regex("[0-9]+", |lex| lex.slice().parse::<isize>().unwrap())]
    Nat(isize),
    #[regex(r"[_A-Za-z][_\w]*", |lex| lex.slice().to_string(), priority = 0)]
    Var(String),
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Multiply,
    #[token("/")]
    Divide,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LSqBra,
    #[token("]")]
    RSqBra,
    #[token("{")]
    LCurBra,
    #[token("}")]
    RCurBra,
    #[token("=")]
    Equals,
    #[token("<")]
    LessThan,
    #[token(">")]
    GreaterThan,
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token("&")]
    Amp,
    #[token("&&")]
    And,
    #[token("!")]
    Bang,
    #[token("let", priority = 1)]
    Let,
    #[token("mut", priority = 1)]
    Mut,
    #[token("alloc", priority = 1)]
    Alloc,
    #[token("free", priority = 1)]
    Free,
    #[token("if", priority = 1)]
    If,
    #[token("then", priority = 1)]
    Then,
    #[token("else", priority = 1)]
    Else,
    #[token("while", priority = 1)]
    While,
    #[token("do", priority = 1)]
    Do,
    #[token("sizeof", priority = 1)]
    Sizeof,
    #[token("true", priority = 1)]
    True,
    #[token("false", priority = 1)]
    False,
    #[token("int", priority = 1)]
    Int,
    #[token("bool", priority = 1)]
    Bool,
    #[token("Ref", priority = 1)]
    Ref,
    #[token("Loc", priority = 1)]
    Loc,
    #[token("skip", priority = 1)]
    Skip,
    #[regex(r"[ \t\f\n]+")]
    Whitespace,
    Error,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Nat(i) => write!(f, "{}", i),
            Self::Var(s) => write!(f, "{}", s),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Multiply => write!(f, "*"),
            Self::Divide => write!(f, "/"),
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::Whitespace => write!(f, "<whitespace>"),
            Self::Equals => write!(f, "="),
            Self::LessThan => write!(f, "<"),
            Self::GreaterThan => write!(f, ">"),
            Self::Semicolon => write!(f, ";"),
            Self::Colon => write!(f, ":"),
            Self::And => write!(f, "&&"),
            Self::Bang => write!(f, "!"),
            Self::Let => write!(f, "let"),
            Self::Mut => write!(f, "mut"),
            Self::Alloc => write!(f, "alloc"),
            Self::Free => write!(f, "free"),
            Self::If => write!(f, "if"),
            Self::Then => write!(f, "then"),
            Self::Else => write!(f, "else"),
            Self::While => write!(f, "while"),
            Self::Do => write!(f, "do"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Error => write!(f, "<error>"),
            Self::Int => write!(f, "int"),
            Self::Bool => write!(f, "bool"),
            Self::Ref => write!(f, "Ref"),
            Self::Loc => write!(f, "Loc"),
            Self::LSqBra => write!(f, "["),
            Self::RSqBra => write!(f, "]"),
            Self::Comma => write!(f, ","),
            Self::Sizeof => write!(f, "sizeof"),
            Self::Amp => write!(f, "&"),
            Self::Skip => write!(f, "skip"),
            Self::LCurBra => write!(f, "{{"),
            Self::RCurBra => write!(f, "}}"),
        }
    }
}
