use crate::{error_handling::Span, keywords::Keyword};

#[derive(Debug, Clone, Copy)]
pub enum TokenKind {
    Keyword(Keyword),
    Operator(Operator),
    Delimiter(Delimiter),
}

#[derive(Debug, Clone, Copy)]
pub enum Operator {
    Plus,
    PlusEqual,
    Minus,
    MinusEqual,
    Star,
    StarEqual,
    Slash,
    SlashEqual,
    Equal,
    EqualEqual,
    Bang,
    BangEqual,
}

#[derive(Debug, Clone, Copy)]
pub enum Delimiter {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
}

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}
