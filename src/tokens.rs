use crate::{error_handling::Span, keywords::Keyword};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Number(String),
    Identifier(String),
    Keyword(Keyword),
    Operator(Operator),
    Delimiter(Delimiter),
    Special(Special),
}

impl TokenKind {
    pub fn is_keyword(&self, expected: Keyword) -> bool {
        match self {
            TokenKind::Keyword(self_kw) => *self_kw == expected,
            _ => false,
        }
    }

    pub fn is_operator(&self, expected: Operator) -> bool {
        match self {
            TokenKind::Operator(self_op) => *self_op == expected,
            _ => false,
        }
    }

    pub fn is_delimiter(&self, expected: Delimiter) -> bool {
        match self {
            TokenKind::Delimiter(self_delimiter) => *self_delimiter == expected,
            _ => false,
        }
    }

    pub fn is_special(&self, expected: Special) -> bool {
        match self {
            TokenKind::Special(self_kind) => *self_kind == expected,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Delimiter {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Special {
    Eof,
    Error(String),
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}
