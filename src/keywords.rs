#[derive(Debug, Clone, Copy)]
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

impl TryInto<Keyword> for String {
    type Error = String;

    fn try_into(self) -> Result<Keyword, Self::Error> {
        match self.as_ref() {
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
            _ => Err(format!("{self} is not a keyword")),
        }
    }
}
