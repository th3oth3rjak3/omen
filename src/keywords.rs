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
    Fun,
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
            "fun" => Ok(Keyword::Fun),
            _ => Err(format!("{value} is not a keyword")),
        }
    }
}
