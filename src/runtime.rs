use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    ast::{BinaryOperator, Expression, Program, Statement, UnaryOperator},
    heap::Heap,
    object::{Function, HeapObject, Object},
    value::Value,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    pub scopes: Vec<Rc<RefCell<HashMap<String, Value>>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            scopes: vec![Rc::new(RefCell::new(HashMap::new()))], // Start with global scope
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Rc::new(RefCell::new(HashMap::new())));
    }

    pub fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.borrow_mut().insert(name, value);
        }
    }

    pub fn get(&self, name: &str) -> Result<Value, String> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.borrow().get(name) {
                return Ok(value.clone());
            }
        }
        Err(format!("Undefined variable '{}'", name))
    }

    pub fn assign(&mut self, name: &str, value: Value) -> Result<Option<Value>, String> {
        for scope in self.scopes.iter().rev() {
            if scope.borrow().contains_key(name) {
                scope.borrow_mut().insert(name.to_string(), value);
                return Ok(None);
            }
        }
        Err(format!("Undefined variable '{}'", name))
    }
}

pub struct Interpreter {
    environment: Environment,
    heap: Heap,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            environment: Environment::new(),
            heap: Heap::new(1024),
        }
    }

    pub fn interpret(&mut self, program: &Program) -> Result<(), String> {
        for statement in &program.statements {
            self.execute_statement(statement)?;
        }
        Ok(())
    }

    fn allocate_object(&mut self, object: Object) -> Rc<RefCell<HeapObject>> {
        self.heap.allocate(object)
    }

    fn execute_statement(&mut self, stmt: &Statement) -> Result<Option<Value>, String> {
        match stmt {
            Statement::ExpressionStatement(expr) => {
                // Evaluate the expression but discard the result
                self.evaluate_expression(expr)?;
                Ok(None)
            }
            Statement::VarDeclaration {
                name, initializer, ..
            } => {
                let value = self.evaluate_expression(initializer)?;
                self.environment.define(name.clone(), value);
                Ok(None)
            }
            Statement::Assignment { name, value } => {
                let val = self.evaluate_expression(value)?;
                self.environment.assign(name, val)
            }
            Statement::Print(expr) => {
                let value = self.evaluate_expression(expr)?;
                println!("{}", self.value_to_string(&value));
                Ok(None)
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition_value = self.evaluate_expression(condition)?;
                let should_execute = match condition_value {
                    Value::Bool(b) => b,
                    _ => return Err("If condition must be boolean".to_string()),
                };

                let mut result: Option<Value> = None;

                if should_execute {
                    result = self.execute_statement(then_branch)?;
                } else if let Some(else_stmt) = else_branch {
                    result = self.execute_statement(else_stmt)?;
                }
                Ok(result)
            }
            Statement::Block(statements) => {
                for stmt in statements {
                    let result = self.execute_statement(stmt)?;
                    if let Some(v) = result {
                        return Ok(Some(v));
                    }
                }
                Ok(None)
            }
            Statement::While { condition, body } => {
                loop {
                    let condition_value = self.evaluate_expression(condition)?;
                    let should_continue = match condition_value {
                        Value::Bool(b) => b,
                        _ => return Err("While condition must be boolean".to_string()),
                    };

                    if !should_continue {
                        break;
                    }

                    self.execute_statement(body)?;
                }
                Ok(None)
            }
            Statement::FunctionDeclaration {
                name,
                parameters,
                return_type,
                body,
            } => {
                let function_obj = Object::Function(Function {
                    name: name.clone(),
                    parameters: parameters.clone(),
                    return_type: return_type.clone(),
                    body: *body.clone(),
                    closure_env: Rc::new(RefCell::new(self.environment.clone())), // Capture current environment
                });

                let heap_obj = self.allocate_object(function_obj);
                self.environment
                    .define(name.clone(), Value::Object(heap_obj));

                Ok(None)
            }
            Statement::Return(expr_opt) => {
                let return_value = match expr_opt {
                    Some(expr) => self.evaluate_expression(expr)?,
                    None => Value::Nil,
                };

                Ok(Some(return_value)) // Use Some to indicate early return
            }
        }
    }

    fn evaluate_expression(&mut self, expr: &Expression) -> Result<Value, String> {
        match expr {
            Expression::Number(n) => Ok(Value::Number(*n)),
            Expression::Bool(b) => Ok(Value::Bool(*b)),
            Expression::String(s) => Ok(Value::String(s.clone())),
            Expression::Nil => Ok(Value::Nil),
            Expression::Identifier(name) => self.environment.get(name),
            Expression::Unary { operator, operand } => {
                let val = self.evaluate_expression(operand)?;
                self.evaluate_unary(operator, val)
            }
            Expression::Binary {
                left,
                operator,
                right,
            } => {
                let left_val = self.evaluate_expression(left)?;
                let right_val = self.evaluate_expression(right)?;
                self.evaluate_binary(&left_val, operator, &right_val)
            }
            Expression::Call { name, arguments } => {
                let function_value = self.environment.get(name)?;

                let function = match function_value {
                    Value::Object(obj) => {
                        let obj_borrow = obj.borrow();
                        match &obj_borrow.object {
                            Object::Function(Function {
                                parameters,
                                body,
                                closure_env,
                                ..
                            }) => (parameters.clone(), body.clone(), closure_env.clone()),
                            _ => return Err(format!("'{}' is not a function", name)),
                        }
                    }
                    _ => return Err(format!("'{}' is not a function", name)),
                };

                let (parameters, body, closure_env) = function;

                if arguments.len() != parameters.len() {
                    return Err(format!(
                        "Function '{}' expects {} arguments, got {}",
                        name,
                        parameters.len(),
                        arguments.len()
                    ));
                }

                // Create new environment starting with closure environment
                let mut call_env = (*closure_env.borrow()).clone();
                call_env.push_scope();

                // Evaluate arguments and bind to parameters
                for (i, p) in parameters.iter().enumerate() {
                    let arg_value = self.evaluate_expression(&arguments[i])?;
                    call_env.define(p.name.clone(), arg_value);
                }

                // Save current environment and switch to call environment
                let saved_env = std::mem::replace(&mut self.environment, call_env);

                // Execute function body
                let result = self.execute_statement(&body);

                // Restore environment
                self.environment = saved_env;

                match result {
                    Ok(Some(return_value)) => Ok(return_value), // Function returned a value
                    Ok(None) => Ok(Value::Nil),                 // Function completed without return
                    Err(e) => Err(e),
                }
            }
        }
    }

    fn evaluate_unary(&self, operator: &UnaryOperator, operand: Value) -> Result<Value, String> {
        match operator {
            UnaryOperator::Not => match operand {
                Value::Bool(b) => Ok(Value::Bool(!b)),
                _ => Err("Cannot apply '!' to non-boolean value".to_string()),
            },
            UnaryOperator::Negate => match operand {
                Value::Number(n) => Ok(Value::Number(-n)),
                _ => Err("Cannot negate non-number value".to_string()),
            },
        }
    }

    fn evaluate_binary(
        &self,
        left: &Value,
        operator: &BinaryOperator,
        right: &Value,
    ) -> Result<Value, String> {
        match operator {
            // Arithmetic operators
            BinaryOperator::Add => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l + r)),
                (Value::String(l), Value::String(r)) => Ok(Value::String(format!("{}{}", l, r))),
                _ => Err("Cannot add incompatible types".to_string()),
            },
            BinaryOperator::Subtract => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l - r)),
                _ => Err("Cannot subtract non-number values".to_string()),
            },
            BinaryOperator::Multiply => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l * r)),
                _ => Err("Cannot multiply non-number values".to_string()),
            },
            BinaryOperator::Divide => match (left, right) {
                (Value::Number(l), Value::Number(r)) => {
                    if *r == 0.0 {
                        Err("Division by zero".to_string())
                    } else {
                        Ok(Value::Number(l / r))
                    }
                }
                _ => Err("Cannot divide non-number values".to_string()),
            },

            // Comparison operators
            BinaryOperator::Less => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l < r)),
                _ => Err("Cannot compare non-number values with <".to_string()),
            },
            BinaryOperator::LessEqual => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l <= r)),
                _ => Err("Cannot compare non-number values with <=".to_string()),
            },
            BinaryOperator::Greater => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l > r)),
                _ => Err("Cannot compare non-number values with >".to_string()),
            },
            BinaryOperator::GreaterEqual => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l >= r)),
                _ => Err("Cannot compare non-number values with >=".to_string()),
            },

            // Equality operators
            BinaryOperator::Equal => Ok(Value::Bool(self.values_equal(left, right))),
            BinaryOperator::NotEqual => Ok(Value::Bool(!self.values_equal(left, right))),

            // Logical operators - require explicit booleans
            BinaryOperator::LogicalAnd => match (left, right) {
                (Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(*l && *r)),
                _ => Err("Logical 'and' requires boolean operands".to_string()),
            },
            BinaryOperator::LogicalOr => match (left, right) {
                (Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(*l || *r)),
                _ => Err("Logical 'or' requires boolean operands".to_string()),
            },
        }
    }

    // Helper methods
    fn values_equal(&self, left: &Value, right: &Value) -> bool {
        match (left, right) {
            (Value::Number(l), Value::Number(r)) => l == r,
            (Value::Bool(l), Value::Bool(r)) => l == r,
            (Value::String(l), Value::String(r)) => l == r,
            (Value::Nil, Value::Nil) => true,
            _ => false,
        }
    }

    fn value_to_string(&self, value: &Value) -> String {
        match value {
            Value::Number(n) => n.to_string(),
            Value::Bool(b) => b.to_string(),
            Value::String(s) => s.clone(),
            Value::Nil => "nil".to_string(),
            _ => "object".into(),
        }
    }
}
