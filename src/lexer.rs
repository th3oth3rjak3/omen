use crate::tokens::Token;

#[derive(Debug, Clone)]
pub struct Lexer {
    pub source: Vec<char>,
    pub line: usize,
    pub column: usize,
    pub offset: usize,
    pub line_offset: usize,
}

impl Default for Lexer {
    fn default() -> Self {
        Self {
            source: Vec::new(),
            line: 1,
            column: 0,
            offset: 0,
            line_offset: 0,
        }
    }
}

impl Lexer {
    pub fn tokenize(&mut self) -> Vec<Token> {
        todo!()
    }

    pub fn next_token(&mut self) -> Token {
        todo!()
    }

    fn is_at_end(&mut self) -> bool {
        self.offset >= self.source.len()
    }

    fn advance(&mut self) {
        todo!()
    }

    fn peek(&mut self) -> Option<Token> {
        todo!()
    }

    fn peek_next(&mut self) -> Option<Token> {
        todo!()
    }

    fn peek_at(&mut self, offset: usize) -> Option<Token> {
        todo!()
    }
}
