use std::fmt::write;

use logos::Logos;

/* ANCHOR: tokens */
#[derive(Logos, Debug, PartialEq, Eq, Hash, Clone)]
pub enum Token {
    // values
    #[regex("[0-9]+", |lex| lex.slice().parse::<isize>().unwrap())]
    Nat(isize),
    #[token("true", priority = 1)]
    True,
    #[token("false", priority = 1)]
    False,
    // #[regex(r"\(\s*\)", priority = 1)]
    // Unit,
    // names
    #[regex(r"[_A-Za-z][_\w]*", |lex| lex.slice().to_string(), priority = 0)]
    Var(String),
    // arithmetic operators
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Divide,
    // boolean operators
    #[token("==")]
    CmpEq,
    #[token("<")]
    LessThan,
    #[token(">")]
    GreaterThan,
    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("!")]
    Bang,
    // delimiters
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LCurBra,
    #[token("}")]
    RCurBra,
    // expr operators
    #[token("&")]
    Amp,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token("@", priority = 1)]
    At,
    // command operators
    #[token(";")]
    Semicolon,
    #[token("=")]
    Equals,
    #[token("::")]
    DColon,
    #[token(":")]
    Colon,
    #[token(";;")]
    DScolon,
    // command constructs
    #[token("type", priority = 1)]
    TypeDef,
    #[token("impl", priority = 1)]
    Impl,
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
    #[token("else", priority = 1)]
    Else,
    #[token("while", priority = 1)]
    While,
    // types
    #[token("int", priority = 1)]
    TInt,
    #[token("bool", priority = 1)]
    TBool,
    #[token("unit", priority = 1)]
    TUnit,
    #[token("Ref", priority = 1)]
    TRef,
    #[token("RefMut", priority = 1)]
    TRefMut,
    #[token("Loc", priority = 1)]
    TLoc,
    #[token("skip", priority = 1)]
    Skip,
    #[regex(r"[ \t\f\n]+")]
    Whitespace,
    // Functions
    #[token("fn", priority = 1)]
    FnDef,
    #[token("return", priority = 1)]
    Return,
    // Impl
    #[token("[")]
    LSqBra,
    #[token("]")]
    RSqBra,
    #[token("pre", priority = 1)]
    Pre,
    #[token("post", priority = 1)]
    Post,
    #[token("lemma", priority = 1)]
    Lemma,
    // HeapPred
    #[token("not", priority = 1)]
    Hnot,
    #[token("or", priority = 1)]
    Hor,
    #[token("and", priority = 1)]
    Hand,
    #[token("implies", priority = 1)]
    Himplies,
    #[token("forall", priority = 1)]
    Hforall,
    #[token("exists", priority = 1)]
    Hexists,
    #[token("emp", priority = 1)]
    Hemp,
    #[token("|->", priority = 1)]
    Hpointsto,
    #[token("**", priority = 1)]
    Hsepconj,
    #[token("-*", priority = 1)]
    Hmagicwand,
    Error
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            // Self::Unit => write!(f, "()"),
            Self::Nat(i) => write!(f, "{}", i),
            Self::Var(s) => write!(f, "{}", s),
            Self::At => write!(f, "@"),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Dot => write!(f, "."),
            Self::Impl => write!(f, "impl"),
            Self::Star => write!(f, "*"),
            Self::Divide => write!(f, "/"),
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::Whitespace => write!(f, "<whitespace>"),
            Self::Equals => write!(f, "="),
            Self::LessThan => write!(f, "<"),
            Self::GreaterThan => write!(f, ">"),
            Self::Semicolon => write!(f, ";"),
            Self::DScolon => write!(f, ";;"),
            Self::Colon => write!(f, ":"),
            Self::And => write!(f, "&&"),
            Self::Bang => write!(f, "!"),
            Self::Let => write!(f, "let"),
            Self::Mut => write!(f, "mut"),
            Self::Alloc => write!(f, "alloc"),
            Self::Free => write!(f, "free"),
            Self::If => write!(f, "if"),
            Self::Else => write!(f, "else"),
            Self::While => write!(f, "while"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Error => write!(f, "<error>"),
            Self::TRef => write!(f, "Ref"),
            Self::TRefMut => write!(f, "RefMut"),
            Self::TLoc => write!(f, "Loc"),
            Self::Comma => write!(f, ","),
            Self::Amp => write!(f, "&"),
            Self::Skip => write!(f, "skip"),
            Self::LCurBra => write!(f, "{{"),
            Self::RCurBra => write!(f, "}}"),
            Self::TInt => write!(f, "int"),
            Self::TBool => write!(f, "bool"),
            Self::TUnit => write!(f, "unit"),
            Self::CmpEq => write!(f, "=="),
            Self::Or => write!(f, "||"),
            Self::TypeDef => write!(f, "type"),
            Self::DColon => write!(f, "::"),
            Self::Hnot => write!(f, "not"),
            Self::Hor => write!(f, "or"),
            Self::Hand => write!(f, "and"),
            Self::Himplies => write!(f, "implies"),
            Self::Hforall => write!(f, "forall"),
            Self::Hexists => write!(f, "exists"),
            Self::Hemp => write!(f, "emp"),
            Self::Hpointsto => write!(f, "|->"),
            Self::Hsepconj => write!(f, "**"),
            Self::Hmagicwand => write!(f, "-*"),
            Self::FnDef => write!(f, "fn"),
            Self::Return => write!(f, "return"),
            Self::LSqBra => write!(f, "["),
            Self::RSqBra => write!(f, "]"),
            Self::Lemma => write!(f, "lemma"),
            Self::Pre => write!(f, "pre"),
            Self::Post => write!(f, "post"),
        }
    }
}
