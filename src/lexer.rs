use crate::{
    error_handling::{Position, Span},
    keywords::Keyword,
    tokens::{Delimiter, Operator, Special, Token, TokenKind},
};

#[derive(Debug, Clone)]
pub struct Lexer {
    pub source: Vec<char>,
    pub line: usize,
    pub column: usize,
    pub offset: usize,
    pub line_offset: usize,
}

impl Lexer {
    pub fn new(source: impl Into<String>) -> Self {
        Self {
            source: source.into().chars().collect(),
            line: 1,
            column: 0,
            offset: 0,
            line_offset: 0,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        loop {
            let tok = self.next_token();
            let kind = tok.kind.clone();
            tokens.push(tok);

            if kind.is_special(Special::Eof) {
                return tokens;
            }
        }
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace_and_comments();

        let current = self.peek();
        if current == '\0' {
            return Token::new(
                TokenKind::Special(Special::Eof),
                Span::new(self.position(), None),
            );
        }

        if current.is_ascii_alphabetic() || current == '_' {
            return self.handle_identifier();
        }

        if current.is_ascii_digit() {
            return self.handle_numbers();
        }

        if current == '"' {
            return self.handle_string_literal();
        }

        match current {
            // delimiters
            '(' => self.make_single(TokenKind::Delimiter(Delimiter::LeftParen)),
            ')' => self.make_single(TokenKind::Delimiter(Delimiter::RightParen)),
            '[' => self.make_single(TokenKind::Delimiter(Delimiter::LeftBracket)),
            ']' => self.make_single(TokenKind::Delimiter(Delimiter::RightBracket)),
            '{' => self.make_single(TokenKind::Delimiter(Delimiter::LeftBrace)),
            '}' => self.make_single(TokenKind::Delimiter(Delimiter::RightBrace)),
            ':' => self.make_single(TokenKind::Delimiter(Delimiter::Colon)),
            ';' => self.make_single(TokenKind::Delimiter(Delimiter::Semicolon)),
            ',' => self.make_single(TokenKind::Delimiter(Delimiter::Comma)),

            // operators
            '+' => match self.peek_next() {
                '=' => self.make_double(TokenKind::Operator(Operator::PlusEqual)),
                _ => self.make_single(TokenKind::Operator(Operator::Plus)),
            },
            '-' => match self.peek_next() {
                '=' => self.make_double(TokenKind::Operator(Operator::MinusEqual)),
                '>' => self.make_double(TokenKind::Operator(Operator::Arrow)),
                _ => self.make_single(TokenKind::Operator(Operator::Minus)),
            },
            '*' => match self.peek_next() {
                '=' => self.make_double(TokenKind::Operator(Operator::StarEqual)),
                _ => self.make_single(TokenKind::Operator(Operator::Star)),
            },
            '/' => match self.peek_next() {
                '=' => self.make_double(TokenKind::Operator(Operator::SlashEqual)),
                _ => self.make_single(TokenKind::Operator(Operator::Slash)),
            },
            '=' => match self.peek_next() {
                '=' => self.make_double(TokenKind::Operator(Operator::EqualEqual)),
                '>' => self.make_double(TokenKind::Operator(Operator::FatArrow)),
                _ => self.make_single(TokenKind::Operator(Operator::Equal)),
            },
            '!' => match self.peek_next() {
                '=' => self.make_double(TokenKind::Operator(Operator::BangEqual)),
                _ => self.make_single(TokenKind::Operator(Operator::Bang)),
            },
            '<' => match self.peek_next() {
                '=' => self.make_double(TokenKind::Operator(Operator::LessEqual)),
                _ => self.make_single(TokenKind::Operator(Operator::Less)),
            },
            '>' => match self.peek_next() {
                '=' => self.make_double(TokenKind::Operator(Operator::GreaterEqual)),
                _ => self.make_single(TokenKind::Operator(Operator::Greater)),
            },
            '.' => self.make_single(TokenKind::Operator(Operator::Dot)),
            '?' => self.make_single(TokenKind::Operator(Operator::Question)),
            _ => self.make_error_token(format!("Unexpected character '{current}'")),
        }
    }

    fn handle_identifier(&mut self) -> Token {
        let start = self.position();
        self.advance(); // skip the first char which is a valid alpha or _ char
        while !self.is_at_end() && (self.peek().is_ascii_alphanumeric() || self.peek() == '_') {
            self.advance();
        }

        let span = Span::new(start, Some(self.position()));

        let literal = self.source[start.offset..self.offset]
            .to_vec()
            .iter()
            .collect::<String>();

        let kw = Keyword::try_from(literal.as_ref());
        if let Ok(kw) = kw {
            return Token::new(TokenKind::Keyword(kw), span);
        }

        Token::new(TokenKind::Identifier(literal), span)
    }

    fn handle_numbers(&mut self) -> Token {
        let start = self.position();
        self.advance(); // skip the first number that we already checked.
        while !self.is_at_end() && self.peek().is_ascii_digit() {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            self.advance(); // skip over period
            while !self.is_at_end() && self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        let span = Span::new(start, Some(self.position()));
        let literal: String = self.source[start.offset..self.offset].iter().collect();
        Token::new(TokenKind::Number(literal), span)
    }

    fn handle_string_literal(&mut self) -> Token {
        self.advance(); // skip "
        let start = self.position();

        while !self.is_at_end() && self.peek() != '"' {
            self.advance();
        }

        if self.peek() != '"' {
            return self.make_error_token("unterminated string literal".into());
        }

        let end = self.position();
        let span = Span::new(start, Some(end));
        let literal = self.source[start.offset..end.offset]
            .iter()
            .collect::<String>();

        let tok = Token::new(TokenKind::String(literal), span);

        self.advance(); // advance over the " for the next token
        tok
    }

    fn position(&mut self) -> Position {
        Position::new(self.line, self.column, self.offset)
    }

    fn is_at_end(&mut self) -> bool {
        self.offset >= self.source.len()
    }

    fn advance(&mut self) {
        match self.peek() {
            '\n' => {
                self.offset += 1;
                self.line += 1;
                self.column = 0;
                self.line_offset = self.offset;
            }
            '\0' => (),
            _ => {
                self.offset += 1;
                self.column += 1;
            }
        }
    }

    fn peek(&mut self) -> char {
        self.peek_at(self.offset)
    }

    fn peek_next(&mut self) -> char {
        self.peek_at(self.offset + 1)
    }

    fn peek_at(&mut self, offset: usize) -> char {
        if offset >= self.source.len() {
            return '\0';
        }

        self.source[offset]
    }

    fn make_error_token(&mut self, err: String) -> Token {
        let start = self.position();
        self.advance();
        let span = Span::new(start, None);
        Token::new(TokenKind::Special(Special::Error(err)), span)
    }

    fn make_single(&mut self, kind: TokenKind) -> Token {
        let start = self.position();
        self.advance();
        let end = Some(self.position());
        let span = Span::new(start, end);
        Token::new(kind, span)
    }

    fn make_double(&mut self, kind: TokenKind) -> Token {
        let start = self.position();
        self.advance();
        self.advance();
        let end = Some(self.position());
        let span = Span::new(start, end);
        Token::new(kind, span)
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.peek() {
                '\r' | '\t' | '\n' | ' ' => self.advance(),
                _ => return,
            }
        }
    }

    fn skip_comments(&mut self) {
        while self.peek() != '\n' {
            self.advance();
        }
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            self.skip_whitespace();
            match (self.peek(), self.peek_next()) {
                ('/', '/') => self.skip_comments(),
                _ => return,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{keywords::Keyword, tokens::Operator};

    use super::*;

    #[test]
    pub fn lexer_can_tokenize_all_delimiters() {
        let source = "()[]{}:;,";
        let mut lexer = Lexer::new(source);

        let tokens = lexer.tokenize();
        assert!(!tokens.is_empty() && tokens.last().unwrap().kind.is_special(Special::Eof));

        assert!(tokens[0].kind.is_delimiter(Delimiter::LeftParen));
        assert!(tokens[1].kind.is_delimiter(Delimiter::RightParen));
        assert!(tokens[2].kind.is_delimiter(Delimiter::LeftBracket));
        assert!(tokens[3].kind.is_delimiter(Delimiter::RightBracket));
        assert!(tokens[4].kind.is_delimiter(Delimiter::LeftBrace));
        assert!(tokens[5].kind.is_delimiter(Delimiter::RightBrace));
        assert!(tokens[6].kind.is_delimiter(Delimiter::Colon));
        assert!(tokens[7].kind.is_delimiter(Delimiter::Semicolon));
        assert!(tokens[8].kind.is_delimiter(Delimiter::Comma));
    }

    #[test]
    pub fn lexer_can_tokenize_all_keywords() {
        let source = r#"
            if 
            else 
            class 
            return 
            continue 
            break 
            let 
            nil 
            while 
            for 
            fn 
            this 
            super
            true
            false    
        "#;

        let mut lexer = Lexer::new(source);

        let tokens = lexer.tokenize();
        assert!(!tokens.is_empty() && tokens.last().unwrap().kind.is_special(Special::Eof));

        assert!(tokens[0].kind.is_keyword(Keyword::If));
        assert!(tokens[1].kind.is_keyword(Keyword::Else));
        assert!(tokens[2].kind.is_keyword(Keyword::Class));
        assert!(tokens[3].kind.is_keyword(Keyword::Return));
        assert!(tokens[4].kind.is_keyword(Keyword::Continue));
        assert!(tokens[5].kind.is_keyword(Keyword::Break));
        assert!(tokens[6].kind.is_keyword(Keyword::Let));
        assert!(tokens[7].kind.is_keyword(Keyword::Nil));
        assert!(tokens[8].kind.is_keyword(Keyword::While));
        assert!(tokens[9].kind.is_keyword(Keyword::For));
        assert!(tokens[10].kind.is_keyword(Keyword::Function));
        assert!(tokens[11].kind.is_keyword(Keyword::This));
        assert!(tokens[12].kind.is_keyword(Keyword::Super));
        assert!(tokens[13].kind.is_keyword(Keyword::True));
        assert!(tokens[14].kind.is_keyword(Keyword::False));
    }

    #[test]
    pub fn lexer_can_tokenize_all_operators() {
        let source = "+ += - -= * *= / /= = == ! != < <= > >= . -> => ?";
        let mut lexer = Lexer::new(source);

        let tokens = lexer.tokenize();
        assert!(!tokens.is_empty() && tokens.last().unwrap().kind.is_special(Special::Eof));

        assert!(tokens[0].kind.is_operator(Operator::Plus));
        assert!(tokens[1].kind.is_operator(Operator::PlusEqual));
        assert!(tokens[2].kind.is_operator(Operator::Minus));
        assert!(tokens[3].kind.is_operator(Operator::MinusEqual));
        assert!(tokens[4].kind.is_operator(Operator::Star));
        assert!(tokens[5].kind.is_operator(Operator::StarEqual));
        assert!(tokens[6].kind.is_operator(Operator::Slash));
        assert!(tokens[7].kind.is_operator(Operator::SlashEqual));
        assert!(tokens[8].kind.is_operator(Operator::Equal));
        assert!(tokens[9].kind.is_operator(Operator::EqualEqual));
        assert!(tokens[10].kind.is_operator(Operator::Bang));
        assert!(tokens[11].kind.is_operator(Operator::BangEqual));
        assert!(tokens[12].kind.is_operator(Operator::Less));
        assert!(tokens[13].kind.is_operator(Operator::LessEqual));
        assert!(tokens[14].kind.is_operator(Operator::Greater));
        assert!(tokens[15].kind.is_operator(Operator::GreaterEqual));
        assert!(tokens[16].kind.is_operator(Operator::Dot));
        assert!(tokens[17].kind.is_operator(Operator::Arrow));
        assert!(tokens[18].kind.is_operator(Operator::FatArrow));
        assert!(tokens[19].kind.is_operator(Operator::Question));
    }

    #[test]
    pub fn lexer_can_produce_number_tokens() {
        let source = "1 1.2 1.0 432";
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize();
        assert!(!tokens.is_empty() && tokens.last().unwrap().kind.is_special(Special::Eof));

        assert_eq!(TokenKind::Number("1".into()), tokens[0].kind);
        assert_eq!(TokenKind::Number("1.2".into()), tokens[1].kind);
        assert_eq!(TokenKind::Number("1.0".into()), tokens[2].kind);
        assert_eq!(TokenKind::Number("432".into()), tokens[3].kind);
    }

    #[test]
    pub fn lexer_produces_eof_token_for_empty_input() {
        let mut lexer = Lexer::new("");
        let tokens = lexer.tokenize();
        assert!(!tokens.is_empty() && tokens.last().unwrap().kind.is_special(Special::Eof));
    }

    #[test]
    pub fn lexer_handles_string_literals() {
        let mut lexer = Lexer::new(r#""a raw string""#);
        let tokens = lexer.tokenize();
        assert_eq!(2, tokens.len());
        assert_eq!(TokenKind::String("a raw string".into()), tokens[0].kind);
    }
}
