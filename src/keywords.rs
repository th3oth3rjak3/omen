#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    If,
    Else,
    Class,
    Return,
    Continue,
    Break,
    Let,
    Nil,
    While,
    For,
    Function,
    This,
    Super,
    True,
    False,
    And,
    Or,
}

impl TryFrom<&str> for Keyword {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "if" => Ok(Keyword::If),
            "else" => Ok(Keyword::Else),
            "class" => Ok(Keyword::Class),
            "return" => Ok(Keyword::Return),
            "continue" => Ok(Keyword::Continue),
            "break" => Ok(Keyword::Break),
            "let" => Ok(Keyword::Let),
            "nil" => Ok(Keyword::Nil),
            "while" => Ok(Keyword::While),
            "for" => Ok(Keyword::For),
            "fn" => Ok(Keyword::Function),
            "this" => Ok(Keyword::This),
            "super" => Ok(Keyword::Super),
            "true" => Ok(Keyword::True),
            "false" => Ok(Keyword::False),
            "and" => Ok(Keyword::And),
            "or" => Ok(Keyword::Or),
            _ => Err(format!("{value} is not a keyword")),
        }
    }
}
