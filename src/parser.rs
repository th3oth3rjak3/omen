use crate::{
    ast::{BinaryOperator, Expression, Program, Statement, UnaryOperator},
    error_handling::Span,
    keywords::Keyword,
    tokens::{Delimiter, Operator, Special, Token, TokenKind},
    types::Type,
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

    pub fn parse(&mut self) -> Program {
        let mut program = Program::default();

        while !self.is_at_end() {
            let stmt = self.parse_statement();
            program.statements.push(stmt);
        }

        program
    }

    fn parse_statement(&mut self) -> Statement {
        match &self.peek().kind {
            TokenKind::Keyword(Keyword::Let) => self.parse_var_declaration(),
            _ => {
                // Default to expression statement
                let expr = self.parse_expression();
                self.consume_delimiter(Delimiter::Semicolon);
                Statement::ExpressionStatement(expr)
            }
        }
    }

    fn parse_type(&mut self) -> Type {
        let base_type = if let TokenKind::Identifier(type_name) = &self.peek().kind {
            let name = type_name.as_str();
            self.advance();

            match name {
                "Number" => Type::Number,
                "Bool" => Type::Bool,
                "String" => Type::String,
                "Nil" => panic!("Cannot declare variables of type Nil"),
                _ => Type::Class(type_name.clone()),
            }
        } else {
            panic!("Expected type name");
        };

        // Check for optional `?` suffix
        if self.peek().kind.is_operator(Operator::Question) {
            self.advance(); // consume '?'
            Type::Nullable(Box::new(base_type))
        } else {
            base_type
        }
    }

    fn parse_var_declaration(&mut self) -> Statement {
        self.advance(); // consume 'let'

        // Parse variable name
        let name = if let TokenKind::Identifier(id) = &self.peek().kind {
            let n = id.clone();
            self.advance();
            n
        } else {
            panic!("Expected identifier after 'let'");
        };

        // Require colon
        if !self.peek().kind.is_delimiter(Delimiter::Colon) {
            panic!("Expected ':' after variable name");
        }
        self.advance(); // consume ':'

        // Parse type annotation (required)
        let type_annotation = self.parse_type();

        // Require equals sign
        if !self.peek().kind.is_operator(Operator::Equal) {
            panic!("Variable must be initialized - no default values allowed");
        }
        self.advance(); // consume '='

        // Parse initializer expression
        let initializer = self.parse_expression();
        println!("INITIALIZER: {:#?}", initializer);

        // Consume semicolon
        self.consume_delimiter(Delimiter::Semicolon);

        Statement::VarDeclaration {
            name,
            type_annotation,
            initializer,
        }
    }

    fn parse_expression(&mut self) -> Expression {
        self.parse_logical_or()
    }

    fn parse_logical_or(&mut self) -> Expression {
        let mut expr = self.parse_logical_and();

        while self.peek().kind.is_keyword(Keyword::Or) {
            self.advance();
            let right = self.parse_logical_and();
            expr = Expression::Binary {
                left: Box::new(expr),
                operator: BinaryOperator::LogicalOr,
                right: Box::new(right),
            };
        }

        expr
    }

    fn parse_logical_and(&mut self) -> Expression {
        let mut expr = self.parse_equality();

        while self.peek().kind.is_keyword(Keyword::And) {
            self.advance();
            let right = self.parse_equality();
            expr = Expression::Binary {
                left: Box::new(expr),
                operator: BinaryOperator::LogicalAnd,
                right: Box::new(right),
            };
        }

        expr
    }

    fn parse_equality(&mut self) -> Expression {
        let mut expr = self.parse_comparison(); // left operand.
        while let TokenKind::Operator(op) = self.peek().kind {
            match op {
                Operator::EqualEqual => {
                    self.advance(); // consume '=='
                    let right = self.parse_comparison(); // Get right operand
                    expr = Expression::Binary {
                        left: Box::new(expr),
                        operator: BinaryOperator::Equal,
                        right: Box::new(right),
                    };
                }
                Operator::BangEqual => {
                    self.advance(); // consume '!='
                    let right = self.parse_comparison();
                    expr = Expression::Binary {
                        left: Box::new(expr),
                        operator: BinaryOperator::NotEqual,
                        right: Box::new(right),
                    };
                }
                _ => break, // Not a comparison operator
            }
        }

        expr
    }

    fn parse_comparison(&mut self) -> Expression {
        let mut expr = self.parse_term(); // Get left operand

        while let TokenKind::Operator(op) = self.peek().kind {
            match op {
                Operator::Less => {
                    self.advance(); // consume '<'
                    let right = self.parse_term(); // Get right operand
                    expr = Expression::Binary {
                        left: Box::new(expr),
                        operator: BinaryOperator::Less,
                        right: Box::new(right),
                    };
                }
                Operator::LessEqual => {
                    self.advance(); // consume '<='
                    let right = self.parse_term();
                    expr = Expression::Binary {
                        left: Box::new(expr),
                        operator: BinaryOperator::LessEqual,
                        right: Box::new(right),
                    };
                }
                Operator::Greater => {
                    self.advance(); // consume '>'
                    let right = self.parse_term();
                    expr = Expression::Binary {
                        left: Box::new(expr),
                        operator: BinaryOperator::Greater,
                        right: Box::new(right),
                    };
                }
                Operator::GreaterEqual => {
                    self.advance(); // consume '>='
                    let right = self.parse_term();
                    expr = Expression::Binary {
                        left: Box::new(expr),
                        operator: BinaryOperator::GreaterEqual,
                        right: Box::new(right),
                    };
                }
                _ => break, // Not a comparison operator
            }
        }

        expr
    }

    fn parse_term(&mut self) -> Expression {
        let mut expr = self.parse_factor(); // Get left operand

        while let TokenKind::Operator(op) = self.peek().kind {
            match op {
                Operator::Plus => {
                    self.advance(); // consume '+'
                    let right = self.parse_factor(); // Get right operand
                    expr = Expression::Binary {
                        left: Box::new(expr),
                        operator: BinaryOperator::Add,
                        right: Box::new(right),
                    };
                }
                Operator::Minus => {
                    self.advance(); // consume '-'
                    let right = self.parse_factor();
                    expr = Expression::Binary {
                        left: Box::new(expr),
                        operator: BinaryOperator::Subtract,
                        right: Box::new(right),
                    };
                }
                _ => break, // Not a term-level operator
            }
        }

        expr
    }

    fn parse_factor(&mut self) -> Expression {
        let mut expr = self.parse_unary(); // Get left operand

        while let TokenKind::Operator(op) = self.peek().kind {
            match op {
                Operator::Star => {
                    self.advance(); // consume '*'
                    let right = self.parse_unary(); // Get right operand
                    expr = Expression::Binary {
                        left: Box::new(expr),
                        operator: BinaryOperator::Multiply,
                        right: Box::new(right),
                    };
                }
                Operator::Slash => {
                    self.advance(); // consume '/'
                    let right = self.parse_unary();
                    expr = Expression::Binary {
                        left: Box::new(expr),
                        operator: BinaryOperator::Divide,
                        right: Box::new(right),
                    };
                }
                _ => break, // Not a factor-level operator
            }
        }

        expr
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
            TokenKind::String(literal) => self.handle_string_literal(literal),
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

    fn handle_string_literal(&mut self, literal: String) -> Expression {
        self.advance();
        Expression::String(literal)
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
        if let TokenKind::Identifier(name) = &self.peek().kind {
            let identifier = name.clone();
            self.advance();
            Expression::Identifier(identifier)
        } else {
            panic!("Expected identifier");
        }
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
        lexer.source = "1.23;".chars().collect();

        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        let expected = Program {
            statements: vec![Statement::ExpressionStatement(Expression::Number(1.23))],
        };

        assert_eq!(expected, ast);
    }

    #[test]
    pub fn parser_can_parse_simple_booleans() {
        let mut lexer = Lexer::default();
        lexer.source = "true;".chars().collect();

        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();
        let expected = Program {
            statements: vec![Statement::ExpressionStatement(Expression::Bool(true))],
        };

        assert_eq!(expected, ast);
    }

    #[test]
    pub fn parser_can_parse_nil_literals() {
        let mut lexer = Lexer::default();
        lexer.source = "nil;".chars().collect();

        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();
        let expected = Program {
            statements: vec![Statement::ExpressionStatement(Expression::Nil)],
        };

        assert_eq!(expected, ast);
    }

    #[test]
    pub fn parser_can_parse_basic_groupings() {
        let mut lexer = Lexer::default();
        lexer.source = "(true);".chars().collect();

        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        let expected = Program {
            statements: vec![Statement::ExpressionStatement(Expression::Bool(true))],
        };

        assert_eq!(expected, ast);
    }

    #[test]
    pub fn parser_can_parse_unary_logical_not_expressions() {
        let mut lexer = Lexer::default();
        lexer.source = "!true;".chars().collect();

        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        let expected = Program {
            statements: vec![Statement::ExpressionStatement(Expression::Unary {
                operator: UnaryOperator::Not,
                operand: Box::new(Expression::Bool(true)),
            })],
        };

        assert_eq!(expected, ast);
    }

    #[test]
    pub fn parser_can_parse_unary_negation_expressions() {
        let mut lexer = Lexer::default();
        lexer.source = "-5;".chars().collect();

        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        let expected = Program {
            statements: vec![Statement::ExpressionStatement(Expression::Unary {
                operator: UnaryOperator::Negate,
                operand: Box::new(Expression::Number(5.)),
            })],
        };

        assert_eq!(expected, ast);
    }

    #[test]
    pub fn parser_can_parse_simple_binary_factor() {
        let mut lexer = Lexer::default();
        lexer.source = "1 * 2;".chars().collect();

        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        let expected = Program {
            statements: vec![Statement::ExpressionStatement(Expression::Binary {
                left: Box::new(Expression::Number(1.)),
                operator: BinaryOperator::Multiply,
                right: Box::new(Expression::Number(2.)),
            })],
        };

        assert_eq!(expected, ast);
    }

    #[test]
    pub fn parser_can_parse_simple_binary_term() {
        let mut lexer = Lexer::default();
        lexer.source = "1 - 2;".chars().collect();

        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        let expected = Program {
            statements: vec![Statement::ExpressionStatement(Expression::Binary {
                left: Box::new(Expression::Number(1.)),
                operator: BinaryOperator::Subtract,
                right: Box::new(Expression::Number(2.)),
            })],
        };

        assert_eq!(expected, ast);
    }

    #[test]
    pub fn parser_can_parse_simple_comparison() {
        let mut lexer = Lexer::default();
        lexer.source = "5 < 10;".chars().collect();

        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        let expected = Program {
            statements: vec![Statement::ExpressionStatement(Expression::Binary {
                left: Box::new(Expression::Number(5.)),
                operator: BinaryOperator::Less,
                right: Box::new(Expression::Number(10.)),
            })],
        };

        assert_eq!(expected, ast);
    }

    #[test]
    pub fn parser_can_parse_equality_when_equal() {
        let mut lexer = Lexer::default();
        lexer.source = "5 == 10;".chars().collect();

        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        let expected = Program {
            statements: vec![Statement::ExpressionStatement(Expression::Binary {
                left: Box::new(Expression::Number(5.)),
                operator: BinaryOperator::Equal,
                right: Box::new(Expression::Number(10.)),
            })],
        };

        assert_eq!(expected, ast);
    }

    #[test]
    pub fn parser_can_parse_equality_when_not_equal() {
        let mut lexer = Lexer::default();
        lexer.source = "5 != 10;".chars().collect();

        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        let expected = Program {
            statements: vec![Statement::ExpressionStatement(Expression::Binary {
                left: Box::new(Expression::Number(5.)),
                operator: BinaryOperator::NotEqual,
                right: Box::new(Expression::Number(10.)),
            })],
        };

        assert_eq!(expected, ast);
    }

    #[test]
    pub fn parser_can_parse_basic_identifiers() {
        let mut lexer = Lexer::default();
        lexer.source = "someIdent != 10;".chars().collect();

        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        let expected = Program {
            statements: vec![Statement::ExpressionStatement(Expression::Binary {
                left: Box::new(Expression::Identifier("someIdent".into())),
                operator: BinaryOperator::NotEqual,
                right: Box::new(Expression::Number(10.)),
            })],
        };

        assert_eq!(expected, ast);
    }

    #[test]
    pub fn parser_can_parse_logical_and() {
        let mut lexer = Lexer::default();
        lexer.source = "true and false;".chars().collect();

        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        let expected = Program {
            statements: vec![Statement::ExpressionStatement(Expression::Binary {
                left: Box::new(Expression::Bool(true)),
                operator: BinaryOperator::LogicalAnd,
                right: Box::new(Expression::Bool(false)),
            })],
        };

        assert_eq!(expected, ast);
    }

    #[test]
    pub fn parser_can_parse_logical_or() {
        let mut lexer = Lexer::default();
        lexer.source = "false or true;".chars().collect();

        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        let expected = Program {
            statements: vec![Statement::ExpressionStatement(Expression::Binary {
                left: Box::new(Expression::Bool(false)),
                operator: BinaryOperator::LogicalOr,
                right: Box::new(Expression::Bool(true)),
            })],
        };

        assert_eq!(expected, ast);
    }

    #[test]
    pub fn parser_can_parse_let_with_number() {
        let mut lexer = Lexer::default();
        lexer.source = "let x: Number = 42;".chars().collect();

        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        let expected = Program {
            statements: vec![Statement::VarDeclaration {
                name: "x".to_string(),
                type_annotation: Type::Number,
                initializer: Expression::Number(42.0),
            }],
        };

        assert_eq!(expected, ast);
    }

    #[test]
    pub fn parser_can_parse_let_with_bool() {
        let mut lexer = Lexer::default();
        lexer.source = "let flag: Bool = true;".chars().collect();

        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        let expected = Program {
            statements: vec![Statement::VarDeclaration {
                name: "flag".to_string(),
                type_annotation: Type::Bool,
                initializer: Expression::Bool(true),
            }],
        };

        assert_eq!(expected, ast);
    }

    #[test]
    pub fn parser_can_parse_let_with_string() {
        let mut lexer = Lexer::default();
        lexer.source = r#"let name: String = "hello";"#.chars().collect();

        let tokens = lexer.tokenize();

        println!("{:#?}", tokens);

        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        let expected = Program {
            statements: vec![Statement::VarDeclaration {
                name: "name".to_string(),
                type_annotation: Type::String,
                initializer: Expression::String("hello".to_string()), // Assuming you add string literals
            }],
        };

        assert_eq!(expected, ast);
    }

    #[test]
    pub fn parser_can_parse_let_with_expression_initializer() {
        let mut lexer = Lexer::default();
        lexer.source = "let result: Number = 5 + 3 * 2;".chars().collect();

        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        let expected = Program {
            statements: vec![Statement::VarDeclaration {
                name: "result".to_string(),
                type_annotation: Type::Number,
                initializer: Expression::Binary {
                    left: Box::new(Expression::Number(5.0)),
                    operator: BinaryOperator::Add,
                    right: Box::new(Expression::Binary {
                        left: Box::new(Expression::Number(3.0)),
                        operator: BinaryOperator::Multiply,
                        right: Box::new(Expression::Number(2.0)),
                    }),
                },
            }],
        };

        assert_eq!(expected, ast);
    }

    #[test]
    #[should_panic(expected = "Cannot declare variables of type Nil")]
    pub fn parser_rejects_nil_type_declaration() {
        let mut lexer = Lexer::default();
        lexer.source = "let invalid: Nil = nil;".chars().collect();

        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        parser.parse(); // Should panic
    }

    #[test]
    pub fn parser_can_parse_let_with_nullable_type() {
        let mut lexer = Lexer::default();
        lexer.source = "let maybe: String? = nil;".chars().collect();

        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        let expected = Program {
            statements: vec![Statement::VarDeclaration {
                name: "maybe".to_string(),
                type_annotation: Type::Nullable(Box::new(Type::String)),
                initializer: Expression::Nil,
            }],
        };

        assert_eq!(expected, ast);
    }
}
