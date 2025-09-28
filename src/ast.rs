use crate::types::Type;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Default for Program {
    fn default() -> Self {
        Self {
            statements: Default::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDecl {
    pub name: String,
    pub params: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub param_type: Type,
}

impl Parameter {
    pub fn new(name: String, param_type: Type) -> Self {
        Self { name, param_type }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassDecl {
    pub name: String,
    pub body: Vec<ClassItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ClassItem {
    Field { name: String, field_type: String },
    Method(FunctionDecl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    VarDeclaration {
        name: String,
        type_annotation: Option<Type>,
        initializer: Expression,
    },
    Assignment {
        name: String,
        value: Expression,
    },
    ExpressionStatement(Expression),
    Print(Expression),
    If {
        condition: Expression,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    },
    Block(Vec<Statement>),
    While {
        condition: Expression,
        body: Box<Statement>,
    },
    FunctionDeclaration {
        name: String,
        parameters: Vec<Parameter>,
        return_type: Option<Type>,
        body: Box<Statement>,
    },
    Return(Option<Expression>),
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum UnaryOperator {
    Negate,
    Not,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Number(f64),
    Bool(bool),
    String(String),
    Identifier(String),
    Unary {
        operator: UnaryOperator,
        operand: Box<Expression>,
    },
    Binary {
        left: Box<Expression>,
        operator: BinaryOperator,
        right: Box<Expression>,
    },
    Nil,
    Call {
        name: String,
        arguments: Vec<Expression>,
    },
}
