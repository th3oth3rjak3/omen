use crate::{
    ast::{BinaryOperator, Expression, Parameter, Program, Statement, UnaryOperator},
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
            TokenKind::Delimiter(Delimiter::LeftBrace) => self.parse_block_statement(),
            TokenKind::Keyword(Keyword::Let) => self.parse_var_declaration(),
            TokenKind::Keyword(Keyword::Print) => self.parse_print_statement(),
            TokenKind::Keyword(Keyword::If) => self.parse_if_statement(),
            TokenKind::Keyword(Keyword::While) => self.parse_while_statement(),
            TokenKind::Keyword(Keyword::Function) => self.parse_function_declaration(),
            TokenKind::Keyword(Keyword::Return) => self.parse_return_statement(),
            TokenKind::Identifier(ident) if self.peek_next().kind.is_operator(Operator::Equal) => {
                self.parse_assignment(ident.to_owned())
            }
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
        let mut type_annotation: Option<Type> = None;

        // Parse variable name
        let name = if let TokenKind::Identifier(id) = &self.peek().kind {
            let n = id.clone();
            self.advance();
            n
        } else {
            panic!("Expected identifier after 'let'");
        };

        // if we have a colon, then there is a type declaration
        if self.peek().kind.is_delimiter(Delimiter::Colon) {
            self.advance(); // consume ':'
            type_annotation = Some(self.parse_type());
        }

        // Require equals sign
        if !self.peek().kind.is_operator(Operator::Equal) {
            panic!("Variable must be initialized - no default values allowed");
        }
        self.advance(); // consume '='

        // Parse initializer expression
        let initializer = self.parse_expression();

        // Consume semicolon
        self.consume_delimiter(Delimiter::Semicolon);

        Statement::VarDeclaration {
            name,
            type_annotation,
            initializer,
        }
    }

    fn parse_print_statement(&mut self) -> Statement {
        self.advance(); // consume 'print'
        let expr = self.parse_expression();
        self.consume_delimiter(Delimiter::Semicolon);
        Statement::Print(expr)
    }

    fn parse_return_statement(&mut self) -> Statement {
        self.advance(); // consume 'return';
        let mut return_expr: Option<Expression> = None;

        if self.peek().kind.is_delimiter(Delimiter::Semicolon) {
        } else {
            let expr = self.parse_expression();
            return_expr = Some(expr);
        }

        self.consume_delimiter(Delimiter::Semicolon);
        Statement::Return(return_expr)
    }

    fn parse_assignment(&mut self, ident: String) -> Statement {
        self.advance(); // skip over the name
        self.advance(); // skip over the =
        let expr = self.parse_expression();
        self.consume_delimiter(Delimiter::Semicolon);

        Statement::Assignment {
            name: ident,
            value: expr,
        }
    }

    fn parse_if_statement(&mut self) -> Statement {
        self.advance(); // consume 'if'
        let condition = self.parse_expression();
        let then_branch = Box::new(self.parse_statement());

        let else_branch = if self.peek().kind.is_keyword(Keyword::Else) {
            self.advance(); // consume 'else'
            Some(Box::new(self.parse_statement()))
        } else {
            None
        };

        Statement::If {
            condition,
            then_branch,
            else_branch,
        }
    }

    fn parse_while_statement(&mut self) -> Statement {
        self.advance(); // consume 'while'
        let condition = self.parse_expression();
        let body = Box::new(self.parse_statement());
        Statement::While { condition, body }
    }

    fn parse_block_statement(&mut self) -> Statement {
        self.consume_delimiter(Delimiter::LeftBrace);
        let mut statements = Vec::new();

        while !self.peek().kind.is_delimiter(Delimiter::RightBrace) && !self.is_at_end() {
            statements.push(self.parse_statement());
        }

        self.consume_delimiter(Delimiter::RightBrace);
        Statement::Block(statements)
    }

    fn parse_function_declaration(&mut self) -> Statement {
        self.advance(); // consume 'fn'

        let name = if let TokenKind::Identifier(id) = &self.peek().kind {
            let n = id.clone();
            self.advance();
            n
        } else {
            panic!("Expected function name");
        };

        self.consume_delimiter(Delimiter::LeftParen);

        let mut parameters: Vec<Parameter> = Vec::new();
        while !self.peek().kind.is_delimiter(Delimiter::RightParen) {
            let param_name = if let TokenKind::Identifier(id) = &self.peek().kind {
                let n = id.clone();
                self.advance();
                n
            } else {
                panic!("Expected parameter name");
            };

            self.consume_delimiter(Delimiter::Colon);
            let param_type = self.parse_type();

            parameters.push(Parameter::new(param_name, param_type));

            if self.peek().kind.is_delimiter(Delimiter::Comma) {
                self.advance(); // consume comma
            }
        }

        self.consume_delimiter(Delimiter::RightParen);

        let return_type = if self.peek().kind.is_operator(Operator::Arrow) {
            self.advance();
            Some(self.parse_type())
        } else {
            None
        };

        let body = Box::new(self.parse_statement()); // Usually a block

        Statement::FunctionDeclaration {
            name,
            parameters,
            return_type,
            body,
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
            Expression::Unary {
                operator: UnaryOperator::Not,
                operand: Box::new(expr),
            }
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
            TokenKind::Identifier(name) => self.handle_identifier(name),
            TokenKind::Delimiter(Delimiter::LeftParen) => self.handle_grouped_expression(),
            _ => Expression::Nil,
        }
    }

    fn handle_identifier(&mut self, name: String) -> Expression {
        let identifier = name.clone();
        self.advance();

        if self.peek().kind.is_delimiter(Delimiter::LeftParen) {
            // Function call
            self.advance(); // consume '('
            let mut arguments = Vec::new();

            while !self.peek().kind.is_delimiter(Delimiter::RightParen) {
                arguments.push(self.parse_expression());
                if self.peek().kind.is_delimiter(Delimiter::Comma) {
                    self.advance();
                }
            }

            self.consume_delimiter(Delimiter::RightParen);
            Expression::Call {
                name: identifier,
                arguments,
            }
        } else {
            // Variable reference
            Expression::Identifier(identifier)
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
            panic!("Expected {expected:#?}, Got: {kind:#?}");
        }
    }

    fn peek(&self) -> Token {
        let default = Token::new(TokenKind::Special(Special::Eof), Span::default());
        self.tokens
            .get(self.current)
            .map(|thing| thing.to_owned())
            .unwrap_or_else(|| default)
    }

    fn peek_next(&self) -> Token {
        let default = Token::new(TokenKind::Special(Special::Eof), Span::default());
        self.tokens
            .get(self.current + 1)
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
        let source = "1.23;";
        let mut lexer = Lexer::new(source);

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
        let source = "true;";
        let mut lexer = Lexer::new(source);

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
        let source = "nil;";
        let mut lexer = Lexer::new(source);

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
        let source = "(true);";
        let mut lexer = Lexer::new(source);

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
        let source = "!true;";
        let mut lexer = Lexer::new(source);

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
        let source = "-5;";
        let mut lexer = Lexer::new(source);

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
        let source = "1 * 2;";
        let mut lexer = Lexer::new(source);

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
        let source = "1 - 2;";
        let mut lexer = Lexer::new(source);

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
        let source = "5 < 10;";
        let mut lexer = Lexer::new(source);

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
        let source = "5 == 10;";
        let mut lexer = Lexer::new(source);

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
        let source = "5 != 10;";
        let mut lexer = Lexer::new(source);

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
        let source = "someIdent != 10;";
        let mut lexer = Lexer::new(source);

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
        let source = "true and false;";
        let mut lexer = Lexer::new(source);

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
        let source = "false or true;";
        let mut lexer = Lexer::new(source);

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
        let source = "let x: Number = 42;";
        let mut lexer = Lexer::new(source);

        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        let expected = Program {
            statements: vec![Statement::VarDeclaration {
                name: "x".to_string(),
                type_annotation: Some(Type::Number),
                initializer: Expression::Number(42.0),
            }],
        };

        assert_eq!(expected, ast);
    }

    #[test]
    pub fn parser_can_parse_let_with_bool() {
        let source = "let flag: Bool = true;";
        let mut lexer = Lexer::new(source);

        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        let expected = Program {
            statements: vec![Statement::VarDeclaration {
                name: "flag".to_string(),
                type_annotation: Some(Type::Bool),
                initializer: Expression::Bool(true),
            }],
        };

        assert_eq!(expected, ast);
    }

    #[test]
    pub fn parser_can_parse_let_with_string() {
        let source = r#"let name: String = "hello";"#;
        let mut lexer = Lexer::new(source);

        let tokens = lexer.tokenize();

        println!("{tokens:#?}");

        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        let expected = Program {
            statements: vec![Statement::VarDeclaration {
                name: "name".to_string(),
                type_annotation: Some(Type::String),
                initializer: Expression::String("hello".to_string()), // Assuming you add string literals
            }],
        };

        assert_eq!(expected, ast);
    }

    #[test]
    pub fn parser_can_parse_let_with_expression_initializer() {
        let source = "let result: Number = 5 + 3 * 2;";
        let mut lexer = Lexer::new(source);

        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        let expected = Program {
            statements: vec![Statement::VarDeclaration {
                name: "result".to_string(),
                type_annotation: Some(Type::Number),
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
        let source = "let invalid: Nil = nil;";
        let mut lexer = Lexer::new(source);

        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        parser.parse(); // Should panic
    }

    #[test]
    pub fn parser_can_parse_let_with_nullable_type() {
        let source = "let maybe: String? = nil;";
        let mut lexer = Lexer::new(source);

        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        let expected = Program {
            statements: vec![Statement::VarDeclaration {
                name: "maybe".to_string(),
                type_annotation: Some(Type::Nullable(Box::new(Type::String))),
                initializer: Expression::Nil,
            }],
        };

        assert_eq!(expected, ast);
    }

    #[test]
    pub fn parser_can_parse_assignment_statements() {
        let source = "let x: Number = 1; x = 2;";
        let mut lexer = Lexer::new(source);

        let tokens = lexer.tokenize();
        println!("TOKENS: {tokens:#?}");
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        println!("AST: {ast:#?}");

        let expected = Program {
            statements: vec![
                Statement::VarDeclaration {
                    name: "x".to_string(),
                    type_annotation: Some(Type::Number),
                    initializer: Expression::Number(1.),
                },
                Statement::Assignment {
                    name: "x".into(),
                    value: Expression::Number(2.),
                },
            ],
        };

        assert_eq!(expected, ast);
    }
}
