use crate::{
    ast::{Expression, UnaryOperator},
    error_handling::Span,
    keywords::Keyword,
    tokens::{Delimiter, Operator, Special, Token, TokenKind},
};

#[derive(Debug, Clone)]
pub struct Parser {
    current: usize,
    tokens: Vec<Token>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { current: 0, tokens }
    }

    pub fn parse(&mut self) -> Expression {
        self.parse_expression()
    }

    fn parse_expression(&mut self) -> Expression {
        self.parse_equality()
    }

    fn parse_equality(&mut self) -> Expression {
        self.parse_comparison()
    }

    fn parse_comparison(&mut self) -> Expression {
        self.parse_term()
    }

    fn parse_term(&mut self) -> Expression {
        self.parse_factor()
    }

    fn parse_factor(&mut self) -> Expression {
        self.parse_unary()
    }

    fn parse_unary(&mut self) -> Expression {
        let kind = self.peek().kind;

        if kind.is_operator(Operator::Bang) {
            self.advance();
            let expr = self.parse_unary();
            return Expression::Unary {
                operator: UnaryOperator::Not,
                operand: Box::new(expr),
            };
        } else if kind.is_operator(Operator::Minus) {
            self.advance();
            let expr = self.parse_unary();
            return Expression::Unary {
                operator: UnaryOperator::Negate,
                operand: Box::new(expr),
            };
        } else {
            self.parse_primary()
        }
    }

    fn parse_primary(&mut self) -> Expression {
        let kind = self.peek().kind;
        match kind {
            TokenKind::Number(literal) => self.handle_number_literal(literal),
            TokenKind::Keyword(Keyword::True) | TokenKind::Keyword(Keyword::False) => {
                self.handle_boolean_literal()
            }
            TokenKind::Keyword(Keyword::Nil) => self.handle_nil_literal(),
            TokenKind::Identifier(_) => self.handle_identifier(),
            TokenKind::Delimiter(Delimiter::LeftParen) => self.handle_grouped_expression(),
            _ => Expression::Nil,
        }
    }

    fn handle_number_literal(&mut self, literal: String) -> Expression {
        let result = literal.parse::<f64>();
        if let Err(e) = result {
            panic!("ERROR PARSING NUMBER LITERAL: {e}");
        }

        self.advance();
        Expression::Number(result.unwrap())
    }

    fn handle_boolean_literal(&mut self) -> Expression {
        let kind = self.peek().kind;

        let expr = if kind.is_keyword(Keyword::True) {
            Expression::Bool(true)
        } else if kind.is_keyword(Keyword::False) {
            Expression::Bool(false)
        } else {
            unreachable!()
        };

        self.advance();
        expr
    }

    fn handle_nil_literal(&mut self) -> Expression {
        self.advance();
        Expression::Nil
    }

    fn handle_identifier(&mut self) -> Expression {
        todo!("Not ready yet")
    }

    fn handle_grouped_expression(&mut self) -> Expression {
        self.consume_delimiter(Delimiter::LeftParen);
        let expr = self.parse_expression();
        self.consume_delimiter(Delimiter::RightParen);

        expr
    }

    fn advance(&mut self) {
        if self.is_at_end() {
            return;
        }

        self.current += 1;
    }

    fn consume_delimiter(&mut self, expected: Delimiter) {
        let kind = self.peek().kind;
        let ok = match kind {
            TokenKind::Delimiter(actual) => actual == expected,
            _ => false,
        };

        self.advance();

        // TODO: add error handling to parser, we'll just panic instead.
        if !ok {
            panic!("Expected {:#?}, Got: {:#?}", expected, kind);
        }
    }

    fn peek(&self) -> Token {
        let default = Token::new(TokenKind::Special(Special::Eof), Span::default());
        self.tokens
            .get(self.current)
            .map(|thing| thing.to_owned())
            .unwrap_or_else(|| default)
    }

    fn is_at_end(&self) -> bool {
        matches!(self.peek().kind, TokenKind::Special(Special::Eof))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ast::UnaryOperator, lexer::Lexer, parser::Parser};

    #[test]
    pub fn parser_can_parse_simple_numbers() {
        let mut lexer = Lexer::default();
        lexer.source = "1.23".chars().collect();

        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        assert_eq!(Expression::Number(1.23), ast);
    }

    #[test]
    pub fn parser_can_parse_simple_booleans() {
        let mut lexer = Lexer::default();
        lexer.source = "true".chars().collect();

        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        assert_eq!(Expression::Bool(true), ast);
    }

    #[test]
    pub fn parser_can_parse_nil_literals() {
        let mut lexer = Lexer::default();
        lexer.source = "nil".chars().collect();

        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        assert_eq!(Expression::Nil, ast);
    }

    #[test]
    pub fn parser_can_parse_basic_groupings() {
        let mut lexer = Lexer::default();
        lexer.source = "(true)".chars().collect();

        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        assert_eq!(Expression::Bool(true), ast);
    }

    #[test]
    pub fn parser_can_parse_unary_logical_not_expressions() {
        let mut lexer = Lexer::default();
        lexer.source = "!true".chars().collect();

        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        assert_eq!(
            Expression::Unary {
                operator: UnaryOperator::Not,
                operand: Box::new(Expression::Bool(true))
            },
            ast
        );
    }

    #[test]
    pub fn parser_can_parse_unary_negation_expressions() {
        let mut lexer = Lexer::default();
        lexer.source = "-5".chars().collect();

        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        assert_eq!(
            Expression::Unary {
                operator: UnaryOperator::Negate,
                operand: Box::new(Expression::Number(5.))
            },
            ast
        );
    }
}
