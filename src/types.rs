use lazy_static::lazy_static;
use std::collections::HashMap;
use std::sync::RwLock;

use crate::ast::Program;

lazy_static! {
    static ref CLASS_REGISTRY: RwLock<HashMap<String, ClassInfo>> = RwLock::new(HashMap::new());
}

use crate::ast::{BinaryOperator, Expression, Statement, UnaryOperator};

#[derive(Debug, Clone)]
pub struct TypeEnvironment {
    scopes: Vec<HashMap<String, Type>>,
}

impl Default for TypeEnvironment {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeEnvironment {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    pub fn define(&mut self, name: String, type_: Type) {
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.insert(name, type_);
        }
    }

    pub fn get(&self, name: &str) -> Result<Type, String> {
        for scope in self.scopes.iter().rev() {
            if let Some(type_) = scope.get(name) {
                return Ok(type_.clone());
            }
        }
        Err(format!("Undefined variable '{name}'"))
    }

    /// Create a new environment with an extra scope containing the refinements.
    /// This is used to type-check branches under refined assumptions.
    pub fn refine_with(&self, refinements: &HashMap<String, Type>) -> TypeEnvironment {
        let mut new_env = self.clone();
        new_env.push_scope();
        for (k, v) in refinements {
            new_env.define(k.clone(), v.clone());
        }
        new_env
    }

    /// Update an existing binding (searching from innermost scope outward).
    /// Returns true if we found and updated an existing binding, false otherwise.
    /// Use this to _permanently_ update the current environment after a guard.
    pub fn update_binding(&mut self, name: &str, new_type: Type) -> bool {
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), new_type);
                return true;
            }
        }
        false
    }
}

fn analyze_condition(
    cond: &Expression,
    env: &TypeEnvironment,
) -> (HashMap<String, Type>, HashMap<String, Type>) {
    fn merge_maps(a: &mut HashMap<String, Type>, b: &HashMap<String, Type>) {
        for (k, v) in b {
            match a.get(k) {
                Some(existing) if existing != v => {
                    // conflict -> remove refinement (conservative)
                    a.remove(k);
                }
                None => {
                    a.insert(k.clone(), v.clone());
                }
                _ => {}
            }
        }
    }

    match cond {
        Expression::Unary {
            operator: UnaryOperator::Not,
            operand,
        } => {
            let (t, f) = analyze_condition(operand, env);
            (f, t)
        }

        Expression::Binary {
            left,
            operator,
            right,
        } => match operator {
            BinaryOperator::LogicalAnd => {
                let (lt, _lf) = analyze_condition(left, env);
                let (rt, _rf) = analyze_condition(right, env);
                // true: both true refinements apply
                let mut true_ref = lt.clone();
                merge_maps(&mut true_ref, &rt);
                // conservative for false
                let false_ref = HashMap::new();
                (true_ref, false_ref)
            }
            BinaryOperator::LogicalOr => {
                let (_lt, lf) = analyze_condition(left, env);
                let (_rt, rf) = analyze_condition(right, env);
                // false: both false refinements apply
                let mut false_ref = lf.clone();
                merge_maps(&mut false_ref, &rf);
                let true_ref = HashMap::new();
                (true_ref, false_ref)
            }
            BinaryOperator::Equal | BinaryOperator::NotEqual => {
                // handle `ident == nil` and `nil == ident` patterns
                // get an identifier name if present
                let mut true_map = HashMap::new();
                let mut false_map = HashMap::new();

                let try_ident_nil =
                    |ident_expr: &Expression,
                     other_expr: &Expression,
                     is_equal: bool,
                     env: &TypeEnvironment,
                     true_map: &mut HashMap<String, Type>,
                     false_map: &mut HashMap<String, Type>| {
                        if let Expression::Identifier(name) = ident_expr {
                            if let Expression::Nil = other_expr {
                                // Lookup the declared type if possible
                                if let Ok(Type::Nullable(inner)) = env.get(name) {
                                    if is_equal {
                                        // ident == nil => ident is Nil when true; non-null when false
                                        true_map.insert(name.clone(), Type::Nil);
                                        false_map.insert(name.clone(), (*inner).clone());
                                    } else {
                                        // ident != nil => ident is non-null when true; nil when false
                                        true_map.insert(name.clone(), (*inner).clone());
                                        false_map.insert(name.clone(), Type::Nil);
                                    }
                                }
                            }
                        }
                    };

                if *operator == BinaryOperator::Equal {
                    try_ident_nil(left, right, true, env, &mut true_map, &mut false_map);
                    try_ident_nil(right, left, true, env, &mut true_map, &mut false_map);
                } else {
                    try_ident_nil(left, right, false, env, &mut true_map, &mut false_map);
                    try_ident_nil(right, left, false, env, &mut true_map, &mut false_map);
                }

                (true_map, false_map)
            }
            _ => (HashMap::new(), HashMap::new()),
        },

        _ => (HashMap::new(), HashMap::new()),
    }
}

pub fn type_check_program(program: &Program) -> Result<(), String> {
    let mut type_env = TypeEnvironment::new();

    for statement in &program.statements {
        type_check_statement(statement, &mut type_env, None)?;
    }
    Ok(())
}

fn type_check_statement(
    stmt: &Statement,
    env: &mut TypeEnvironment,
    function_return_type: Option<&Type>,
) -> Result<bool, String> {
    match stmt {
        Statement::VarDeclaration {
            name,
            type_annotation,
            initializer,
        } => {
            let init_type = type_check_expression(initializer, env)?;

            let var_type: Type;

            if let Some(type_annotation) = type_annotation {
                // user provided one, so let's use that.
                var_type = type_annotation.to_owned();
            } else {
                if init_type == Type::Nil {
                    return Err("Cannot infer type from 'nil'. Use a type annotation.".into());
                }
                var_type = init_type.clone();
            }

            if !init_type.is_assignable_to(&var_type) {
                return Err(format!(
                    "Cannot assign {} to variable '{}' of type {}",
                    type_to_string(&init_type),
                    name,
                    type_to_string(&var_type)
                ));
            }

            env.define(name.clone(), var_type);
            Ok(false)
        }
        Statement::Assignment { name, value } => {
            let var_type = env.get(name)?;
            let value_type = type_check_expression(value, env)?;
            if !value_type.is_assignable_to(&var_type) {
                return Err(format!(
                    "Cannot assign {} to variable '{}' of type {}",
                    type_to_string(&value_type),
                    name,
                    type_to_string(&var_type)
                ));
            }
            Ok(false)
        }
        Statement::ExpressionStatement(expr) => {
            type_check_expression(expr, env)?;
            Ok(false)
        }
        Statement::Print(expr) => {
            type_check_expression(expr, env)?;
            Ok(false)
        }
        Statement::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let condition_type = type_check_expression(condition, env)?;
            if condition_type != Type::Bool {
                return Err(format!(
                    "If condition must be boolean, got {}",
                    type_to_string(&condition_type)
                ));
            }

            // analyze condition to produce refinements for true / false cases
            let (true_ref, false_ref) = analyze_condition(condition, env);

            // typecheck then under "true" refinements
            let mut then_env = env.refine_with(&true_ref);
            let then_exits =
                type_check_statement(then_branch, &mut then_env, function_return_type)?;

            // typecheck else under "false" refinements if present
            let (else_exits, _else_env) = if let Some(else_stmt) = else_branch {
                let mut else_env = env.refine_with(&false_ref);
                let e_exits = type_check_statement(else_stmt, &mut else_env, function_return_type)?;
                (e_exits, Some(else_env))
            } else {
                (false, None)
            };

            // Common guard-with-early-return pattern:
            // if (cond) { return ... }   // then_exits == true and no else
            // => code after if runs under the *false* refinement (cond == false)
            if then_exits && else_branch.is_none() {
                // permanently apply false refinements to current env for subsequent code
                for (name, ty) in false_ref {
                    // only update binding if the variable already exists in some parent scope
                    let _ = env.update_binding(&name, ty);
                }
            }

            // The if-statement as a whole "always exits" only if both branches exit
            Ok(then_exits && else_exits)
        }
        Statement::While { condition, body } => {
            let condition_type = type_check_expression(condition, env)?;
            if condition_type != Type::Bool {
                return Err(format!(
                    "While condition must be boolean, got {}",
                    type_to_string(&condition_type)
                ));
            }
            type_check_statement(body, env, function_return_type)?;
            Ok(false)
        }
        Statement::Block(statements) => {
            for stmt in statements {
                let exited = type_check_statement(stmt, env, function_return_type)?;
                if exited {
                    return Ok(true);
                }
            }
            Ok(false)
        }

        Statement::FunctionDeclaration {
            name,
            parameters,
            return_type,
            body,
        } => {
            let func_type = Type::Function {
                params: parameters
                    .iter()
                    .map(|p| Box::new(p.param_type.clone()))
                    .collect(),
                return_types: match return_type {
                    Some(t) => vec![Box::new(t.clone())],
                    None => vec![],
                },
            };
            env.define(name.clone(), func_type);

            env.push_scope();
            for p in parameters {
                env.define(p.name.clone(), p.param_type.clone());
            }
            type_check_statement(body, env, return_type.as_ref())?; // Pass return type context
            env.pop_scope();

            Ok(false)
        }
        Statement::Return(expr_opt) => {
            match (expr_opt, function_return_type) {
                (Some(expr), Some(expected_type)) => {
                    let return_type = type_check_expression(expr, env)?;
                    if !return_type.is_assignable_to(expected_type) {
                        return Err(format!(
                            "Return type {} doesn't match function return type {}",
                            type_to_string(&return_type),
                            type_to_string(expected_type)
                        ));
                    }
                }
                (Some(_), None) => {
                    return Err("Cannot return value from void function".to_string());
                }
                (None, Some(_)) => {
                    return Err("Function must return a value".to_string());
                }
                (None, None) => {
                    // Void return from void function - OK
                }
            }
            Ok(true)
        }
    }
}

fn type_check_expression(expr: &Expression, env: &TypeEnvironment) -> Result<Type, String> {
    match expr {
        Expression::Number(_) => Ok(Type::Number),
        Expression::Bool(_) => Ok(Type::Bool),
        Expression::String(_) => Ok(Type::String),
        Expression::Nil => Ok(Type::Nil),
        Expression::Identifier(name) => env.get(name),
        Expression::Unary { operator, operand } => {
            let operand_type = type_check_expression(operand, env)?;
            match operator {
                UnaryOperator::Not => {
                    if operand_type == Type::Bool {
                        Ok(Type::Bool)
                    } else {
                        Err(format!(
                            "Cannot apply '!' to {}",
                            type_to_string(&operand_type)
                        ))
                    }
                }
                UnaryOperator::Negate => {
                    if operand_type == Type::Number {
                        Ok(Type::Number)
                    } else {
                        Err(format!("Cannot negate {}", type_to_string(&operand_type)))
                    }
                }
            }
        }
        Expression::Binary {
            left,
            operator,
            right,
        } => {
            let left_type = type_check_expression(left, env)?;
            let right_type = type_check_expression(right, env)?;

            match operator {
                BinaryOperator::Add => {
                    if left_type == Type::Number && right_type == Type::Number {
                        Ok(Type::Number)
                    } else if left_type == Type::String && right_type == Type::String {
                        Ok(Type::String)
                    } else {
                        Err(format!(
                            "Cannot add {} and {}",
                            type_to_string(&left_type),
                            type_to_string(&right_type)
                        ))
                    }
                }
                BinaryOperator::Subtract | BinaryOperator::Multiply | BinaryOperator::Divide => {
                    if left_type == Type::Number && right_type == Type::Number {
                        Ok(Type::Number)
                    } else {
                        Err(format!(
                            "Cannot apply {:?} to {} and {}",
                            operator,
                            type_to_string(&left_type),
                            type_to_string(&right_type)
                        ))
                    }
                }
                BinaryOperator::Less
                | BinaryOperator::LessEqual
                | BinaryOperator::Greater
                | BinaryOperator::GreaterEqual => {
                    if left_type == Type::Number && right_type == Type::Number {
                        Ok(Type::Bool)
                    } else {
                        Err(format!(
                            "Cannot compare {} and {} with {:?}",
                            type_to_string(&left_type),
                            type_to_string(&right_type),
                            operator
                        ))
                    }
                }
                BinaryOperator::Equal | BinaryOperator::NotEqual => {
                    // Allow equality comparison between same types
                    // Also allow comparing Nullable(T) with Nil (or Nil with Nullable(T))
                    let ok = left_type == right_type
                        || matches!((&left_type, &right_type), (Type::Nullable(_), Type::Nil))
                        || matches!((&left_type, &right_type), (Type::Nil, Type::Nullable(_)));

                    if ok {
                        Ok(Type::Bool)
                    } else {
                        Err(format!(
                            "Cannot compare {} and {} for equality",
                            type_to_string(&left_type),
                            type_to_string(&right_type)
                        ))
                    }
                }

                BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr => {
                    if left_type == Type::Bool && right_type == Type::Bool {
                        Ok(Type::Bool)
                    } else {
                        Err(format!(
                            "Logical {:?} requires boolean operands, got {} and {}",
                            operator,
                            type_to_string(&left_type),
                            type_to_string(&right_type)
                        ))
                    }
                }
            }
        }
        Expression::Call { name, arguments } => {
            let func_type = env.get(name)?;
            if let Type::Function {
                params,
                return_types,
            } = func_type
            {
                if arguments.len() != params.len() {
                    return Err(format!(
                        "Function '{}' expects {} arguments, got {}",
                        name,
                        params.len(),
                        arguments.len()
                    ));
                }

                for (i, arg) in arguments.iter().enumerate() {
                    let arg_type = type_check_expression(arg, env)?;
                    if !arg_type.is_assignable_to(&params[i]) {
                        return Err(format!("Argument {i} type mismatch"));
                    }
                }

                if return_types.len() == 1 {
                    Ok((*return_types[0]).clone())
                } else {
                    Ok(Type::Nil) // Void function
                }
            } else {
                Err(format!("'{name}' is not a function"))
            }
        }
    }
}

fn type_to_string(type_: &Type) -> String {
    match type_ {
        Type::Number => "Number".to_string(),
        Type::Bool => "Bool".to_string(),
        Type::String => "String".to_string(),
        Type::Nil => "Nil".to_string(),
        Type::Class(name) => name.clone(),
        Type::Nullable(inner) => format!("{}?", type_to_string(inner)),
        _ => format!("{type_:?}"), // For complex types not yet implemented
    }
}

struct ClassInfo {
    _name: String,
    superclass: Option<String>,
}

impl ClassInfo {
    fn is_subtype_of(&self, parent: &str) -> bool {
        let mut current = self.superclass.clone();

        while let Some(super_name) = current {
            if super_name == parent {
                return true;
            }
            current = CLASS_REGISTRY
                .read()
                .unwrap()
                .get(&super_name)
                .and_then(|ci| ci.superclass.clone());
        }

        false
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Number,
    Bool,
    String,
    Nil,
    Tuple(Vec<Box<Type>>),
    NamedTuple(Vec<(String, Box<Type>)>),
    Function {
        params: Vec<Box<Type>>,
        return_types: Vec<Box<Type>>,
    },
    // User-defined types.
    Class(String), // String is the fully-qualified class name.
    // Nullable Wrapper
    Nullable(Box<Type>),
}

impl Type {
    fn is_nullable(&self) -> bool {
        if let Type::Nullable(_) = self {
            return true;
        }

        false
    }

    /// Check if this type is assignable to another type.
    pub fn is_assignable_to(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Tuple(elements_1), Type::Tuple(elements_2)) => {
                elements_1.len() == elements_2.len()
                    && elements_1
                        .iter()
                        .zip(elements_2.iter())
                        .all(|(el_1_type, el_2_type)| el_1_type.is_assignable_to(el_2_type))
            }
            (Type::NamedTuple(elements_1), Type::NamedTuple(elements_2)) => {
                elements_1.len() == elements_2.len()
                    && elements_1.iter().zip(elements_2.iter()).all(
                        |((_, el_1_type), (_, el_2_type))| el_1_type.is_assignable_to(el_2_type),
                    )
            }
            (Type::NamedTuple(named_elements), Type::Tuple(elements)) => {
                named_elements.len() == elements.len()
                    && named_elements.iter().zip(elements.iter()).all(
                        |((_, named_element_type), element_type)| {
                            named_element_type.is_assignable_to(element_type)
                        },
                    )
            }

            (Type::Tuple(elements), Type::NamedTuple(named_elements)) => {
                elements.len() == named_elements.len()
                    && elements.iter().zip(named_elements.iter()).all(
                        |(element_type, (_, named_element_type))| {
                            element_type.is_assignable_to(named_element_type)
                        },
                    )
            }
            (
                Type::Function {
                    params: params1,
                    return_types: returns1,
                },
                Type::Function {
                    params: params2,
                    return_types: returns2,
                },
            ) => {
                // Same number of parameters and return values
                if params1.len() != params2.len() || returns1.len() != returns2.len() {
                    return false;
                }

                // Parameters are contravariant: self param type must be a supertype of other param type
                let params_ok = params1
                    .iter()
                    .zip(params2.iter())
                    .all(|(self_param, other_param)| other_param.is_assignable_to(self_param));

                // Return types are covariant: self return type must be a subtype of other return type
                let returns_ok = returns1
                    .iter()
                    .zip(returns2.iter())
                    .all(|(self_ret, other_ret)| self_ret.is_assignable_to(other_ret));

                params_ok && returns_ok
            }

            (Type::Class(a_name), Type::Class(b_name)) => {
                if a_name == b_name {
                    true
                } else {
                    CLASS_REGISTRY
                        .read()
                        .unwrap()
                        .get(a_name)
                        .is_some_and(|class_info| class_info.is_subtype_of(b_name))
                }
            }

            // nil can be assigned to any nullable type
            (Type::Nil, Type::Nullable(_)) => true,

            (Type::Nullable(_), b) if !b.is_nullable() => false,
            (Type::Nullable(a), Type::Nullable(b)) => a.is_assignable_to(b),

            // Allow non-null assignment to nullable of same type.
            (a, Type::Nullable(nullable_inner)) => a.is_assignable_to(nullable_inner),

            (a, b) if a == b => true,

            // Nothing else is assignable.
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_exact_type_matches() {
        // Built-in types should match themselves
        assert!(Type::Number.is_assignable_to(&Type::Number));
        assert!(Type::Bool.is_assignable_to(&Type::Bool));
        assert!(Type::String.is_assignable_to(&Type::String));
        assert!(Type::Nil.is_assignable_to(&Type::Nil));

        // Class types should match themselves
        let point = Type::Class("Point".to_string());
        assert!(point.is_assignable_to(&point));

        // Different classes should not match
        let point = Type::Class("Point".to_string());
        let rectangle = Type::Class("Rectangle".to_string());
        assert!(!point.is_assignable_to(&rectangle));
    }

    #[test]
    fn test_nil_to_nullable_assignment() {
        // nil can be assigned to any nullable type
        assert!(Type::Nil.is_assignable_to(&Type::Nullable(Box::new(Type::Number))));
        assert!(Type::Nil.is_assignable_to(&Type::Nullable(Box::new(Type::String))));
        assert!(Type::Nil.is_assignable_to(&Type::Nullable(Box::new(Type::Bool))));

        // nil can be assigned to nullable class types
        let nullable_point = Type::Nullable(Box::new(Type::Class("Point".to_string())));
        assert!(Type::Nil.is_assignable_to(&nullable_point));

        // nil cannot be assigned to non-nullable types
        assert!(!Type::Nil.is_assignable_to(&Type::Number));
        assert!(!Type::Nil.is_assignable_to(&Type::String));
        assert!(!Type::Nil.is_assignable_to(&Type::Class("Point".to_string())));
    }

    #[test]
    fn test_non_null_to_nullable_assignment() {
        // Built-in types can be assigned to their nullable versions
        assert!(Type::Number.is_assignable_to(&Type::Nullable(Box::new(Type::Number))));
        assert!(Type::String.is_assignable_to(&Type::Nullable(Box::new(Type::String))));
        assert!(Type::Bool.is_assignable_to(&Type::Nullable(Box::new(Type::Bool))));

        // Class types can be assigned to their nullable versions
        let point = Type::Class("Point".to_string());
        let nullable_point = Type::Nullable(Box::new(Type::Class("Point".to_string())));
        assert!(point.is_assignable_to(&nullable_point));

        // Wrong types cannot be assigned to nullable of different type
        assert!(!Type::Number.is_assignable_to(&Type::Nullable(Box::new(Type::String))));
        assert!(!Type::String.is_assignable_to(&Type::Nullable(Box::new(Type::Number))));
    }

    #[test]
    fn test_nullable_cannot_be_assigned_to_non_nullable() {
        // Nullable types cannot be assigned to non-nullable versions
        let nullable_int = Type::Nullable(Box::new(Type::Number));
        assert!(!nullable_int.is_assignable_to(&Type::Number));

        let nullable_string = Type::Nullable(Box::new(Type::String));
        assert!(!nullable_string.is_assignable_to(&Type::String));

        // Nullable classes cannot be assigned to non-nullable classes
        let nullable_point = Type::Nullable(Box::new(Type::Class("Point".to_string())));
        let point = Type::Class("Point".to_string());
        assert!(!nullable_point.is_assignable_to(&point));
    }

    #[test]
    fn test_nullable_type_equality() {
        // Nullable types should be equal to themselves
        let nullable_int = Type::Nullable(Box::new(Type::Number));
        let another_nullable_int = Type::Nullable(Box::new(Type::Number));
        assert!(nullable_int.is_assignable_to(&another_nullable_int));

        // Different nullable types should not be equal
        let nullable_int = Type::Nullable(Box::new(Type::Number));
        let nullable_string = Type::Nullable(Box::new(Type::String));
        assert!(!nullable_int.is_assignable_to(&nullable_string));

        // Nullable class types
        let nullable_point = Type::Nullable(Box::new(Type::Class("Point".to_string())));
        let another_nullable_point = Type::Nullable(Box::new(Type::Class("Point".to_string())));
        assert!(nullable_point.is_assignable_to(&another_nullable_point));
    }

    #[test]
    fn test_incompatible_type_assignments() {
        // Built-in types cannot be assigned to each other
        assert!(!Type::Number.is_assignable_to(&Type::String));
        assert!(!Type::Bool.is_assignable_to(&Type::Number));
        assert!(!Type::String.is_assignable_to(&Type::Bool));

        // Classes cannot be assigned to built-in types
        let point = Type::Class("Point".to_string());
        assert!(!point.is_assignable_to(&Type::Number));
        assert!(!point.is_assignable_to(&Type::String));
        assert!(!Type::String.is_assignable_to(&point));

        // Different classes cannot be assigned to each other
        let point = Type::Class("Point".to_string());
        let rectangle = Type::Class("Rectangle".to_string());
        assert!(!point.is_assignable_to(&rectangle));
        assert!(!rectangle.is_assignable_to(&point));
    }

    #[test]
    fn test_fully_qualified_class_names() {
        // Different fully-qualified names should be different types
        let local_point = Type::Class("Point".to_string());
        let graphics_point = Type::Class("graphics.Point".to_string());
        let math_point = Type::Class("math.Point".to_string());

        assert!(!local_point.is_assignable_to(&graphics_point));
        assert!(!graphics_point.is_assignable_to(&math_point));
        assert!(!math_point.is_assignable_to(&local_point));

        // But same fully-qualified names should match
        let point1 = Type::Class("graphics.Point".to_string());
        let point2 = Type::Class("graphics.Point".to_string());
        assert!(point1.is_assignable_to(&point2));
    }

    #[test]
    fn test_nullable_class_types() {
        let point = Type::Class("Point".to_string());
        let nullable_point = Type::Nullable(Box::new(Type::Class("Point".to_string())));

        // Point can be assigned to Point?
        assert!(point.is_assignable_to(&nullable_point));

        // Point? cannot be assigned to Point
        assert!(!nullable_point.is_assignable_to(&point));

        // nil can be assigned to Point?
        assert!(Type::Nil.is_assignable_to(&nullable_point));
    }

    #[test]
    fn test_tuple_types() {
        // Empty tuple (unit type)
        let unit = Type::Tuple(vec![]);
        assert!(unit.is_assignable_to(&unit));

        // Simple tuple
        let int_string_tuple = Type::Tuple(vec![Box::new(Type::Number), Box::new(Type::String)]);

        // Same tuple structure should match
        let another_int_string_tuple =
            Type::Tuple(vec![Box::new(Type::Number), Box::new(Type::String)]);
        assert!(int_string_tuple.is_assignable_to(&another_int_string_tuple));

        // Different tuple structures should not match
        let string_int_tuple = Type::Tuple(vec![Box::new(Type::String), Box::new(Type::Number)]);
        assert!(!int_string_tuple.is_assignable_to(&string_int_tuple));
    }

    #[test]
    fn test_named_tuple_types() {
        // Named tuple for function return
        let divide_result = Type::NamedTuple(vec![
            ("result".to_string(), Box::new(Type::Number)),
            (
                "error".to_string(),
                Box::new(Type::Nullable(Box::new(Type::String))),
            ),
        ]);

        // Same named tuple should match
        let another_divide_result = Type::NamedTuple(vec![
            ("result".to_string(), Box::new(Type::Number)),
            (
                "error".to_string(),
                Box::new(Type::Nullable(Box::new(Type::String))),
            ),
        ]);
        assert!(divide_result.is_assignable_to(&another_divide_result));

        // Different field names should not match
        let different_names = Type::NamedTuple(vec![
            ("value".to_string(), Box::new(Type::Number)),
            (
                "message".to_string(),
                Box::new(Type::Nullable(Box::new(Type::String))),
            ),
        ]);
        assert!(divide_result.is_assignable_to(&different_names));

        // Different field order should not match
        let different_order = Type::NamedTuple(vec![
            (
                "error".to_string(),
                Box::new(Type::Nullable(Box::new(Type::String))),
            ),
            ("result".to_string(), Box::new(Type::Number)),
        ]);
        assert!(!divide_result.is_assignable_to(&different_order));
    }

    #[test]
    fn test_function_types() {
        // Simple function: (Number, Number) -> Number
        let add_function = Type::Function {
            params: vec![Box::new(Type::Number), Box::new(Type::Number)],
            return_types: vec![Box::new(Type::Number)],
        };

        // Same function signature should match
        let another_add_function = Type::Function {
            params: vec![Box::new(Type::Number), Box::new(Type::Number)],
            return_types: vec![Box::new(Type::Number)],
        };
        assert!(add_function.is_assignable_to(&another_add_function));

        // Different return types should not match
        let string_function = Type::Function {
            params: vec![Box::new(Type::Number), Box::new(Type::Number)],
            return_types: vec![Box::new(Type::String)],
        };
        assert!(!add_function.is_assignable_to(&string_function));

        // Multiple return values
        let multi_return_function = Type::Function {
            params: vec![Box::new(Type::Number)],
            return_types: vec![Box::new(Type::Number), Box::new(Type::String)],
        };

        // Different number of return values should not match
        assert!(!add_function.is_assignable_to(&multi_return_function));
    }

    #[test]
    fn test_nullable_complex_types() {
        // Nullable tuple
        let nullable_tuple = Type::Nullable(Box::new(Type::Tuple(vec![
            Box::new(Type::Number),
            Box::new(Type::String),
        ])));

        // Regular tuple can be assigned to nullable tuple
        let regular_tuple = Type::Tuple(vec![Box::new(Type::Number), Box::new(Type::String)]);
        assert!(regular_tuple.is_assignable_to(&nullable_tuple));

        // nil can be assigned to nullable tuple
        assert!(Type::Nil.is_assignable_to(&nullable_tuple));

        // Nullable function
        let nullable_function = Type::Nullable(Box::new(Type::Function {
            params: vec![Box::new(Type::Number)],
            return_types: vec![Box::new(Type::String)],
        }));

        let regular_function = Type::Function {
            params: vec![Box::new(Type::Number)],
            return_types: vec![Box::new(Type::String)],
        };
        assert!(regular_function.is_assignable_to(&nullable_function));
        assert!(Type::Nil.is_assignable_to(&nullable_function));

        // Nullable named tuple
        let nullable_named_tuple = Type::Nullable(Box::new(Type::NamedTuple(vec![(
            "result".to_string(),
            Box::new(Type::Number),
        )])));

        let regular_named_tuple =
            Type::NamedTuple(vec![("result".to_string(), Box::new(Type::Number))]);
        assert!(regular_named_tuple.is_assignable_to(&nullable_named_tuple));
        assert!(Type::Nil.is_assignable_to(&nullable_named_tuple));
    }

    #[test]
    fn test_tuple_named_tuple_interchangeability() {
        // Plain tuple and named tuple with same structure should be interchangeable
        let plain_tuple = Type::Tuple(vec![
            Box::new(Type::Number),
            Box::new(Type::String),
            Box::new(Type::Bool),
        ]);

        let named_tuple = Type::NamedTuple(vec![
            ("first".to_string(), Box::new(Type::Number)),
            ("second".to_string(), Box::new(Type::String)),
            ("third".to_string(), Box::new(Type::Bool)),
        ]);

        // Both directions should work
        assert!(plain_tuple.is_assignable_to(&named_tuple));
        assert!(named_tuple.is_assignable_to(&plain_tuple));

        // Different lengths should not be assignable
        let shorter_plain = Type::Tuple(vec![Box::new(Type::Number), Box::new(Type::String)]);
        assert!(!plain_tuple.is_assignable_to(&shorter_plain));
        assert!(!named_tuple.is_assignable_to(&shorter_plain));
    }

    #[test]
    fn test_tuple_assignability_with_nullable() {
        // Tuple with non-nullable element
        let plain_tuple = Type::Tuple(vec![Box::new(Type::Number), Box::new(Type::String)]);

        // Tuple with nullable element
        let nullable_tuple = Type::Tuple(vec![
            Box::new(Type::Number),
            Box::new(Type::Nullable(Box::new(Type::String))),
        ]);

        // Non-nullable tuple should be assignable to nullable tuple
        assert!(plain_tuple.is_assignable_to(&nullable_tuple));

        // Nullable tuple should NOT be assignable to non-nullable tuple
        assert!(!nullable_tuple.is_assignable_to(&plain_tuple));

        // Same test with named tuples
        let plain_named = Type::NamedTuple(vec![
            ("id".to_string(), Box::new(Type::Number)),
            ("name".to_string(), Box::new(Type::String)),
        ]);

        let nullable_named = Type::NamedTuple(vec![
            ("id".to_string(), Box::new(Type::Number)),
            (
                "name".to_string(),
                Box::new(Type::Nullable(Box::new(Type::String))),
            ),
        ]);

        assert!(plain_named.is_assignable_to(&nullable_named));
        assert!(!nullable_named.is_assignable_to(&plain_named));
    }

    #[test]
    fn test_function_assignability_exact_match() {
        // Simple function: (Number, String) -> Bool
        let func1 = Type::Function {
            params: vec![Box::new(Type::Number), Box::new(Type::String)],
            return_types: vec![Box::new(Type::Bool)],
        };

        // Exact same signature
        let func2 = Type::Function {
            params: vec![Box::new(Type::Number), Box::new(Type::String)],
            return_types: vec![Box::new(Type::Bool)],
        };

        // Should be assignable to each other
        assert!(func1.is_assignable_to(&func2));
        assert!(func2.is_assignable_to(&func1));
    }

    #[test]
    fn test_function_assignability_parameter_mismatch() {
        let base_func = Type::Function {
            params: vec![Box::new(Type::Number), Box::new(Type::String)],
            return_types: vec![Box::new(Type::Bool)],
        };

        // Different parameter count
        let fewer_params = Type::Function {
            params: vec![Box::new(Type::Number)],
            return_types: vec![Box::new(Type::Bool)],
        };
        assert!(!base_func.is_assignable_to(&fewer_params));
        assert!(!fewer_params.is_assignable_to(&base_func));

        // Different parameter types
        let different_param_types = Type::Function {
            params: vec![Box::new(Type::String), Box::new(Type::String)], // Number -> Number
            return_types: vec![Box::new(Type::Bool)],
        };
        assert!(!base_func.is_assignable_to(&different_param_types));
        assert!(!different_param_types.is_assignable_to(&base_func));

        // Different parameter order
        let swapped_params = Type::Function {
            params: vec![Box::new(Type::String), Box::new(Type::Number)], // Swapped order
            return_types: vec![Box::new(Type::Bool)],
        };
        assert!(!base_func.is_assignable_to(&swapped_params));
        assert!(!swapped_params.is_assignable_to(&base_func));
    }

    #[test]
    fn test_function_assignability_return_type_mismatch() {
        let base_func = Type::Function {
            params: vec![Box::new(Type::Number)],
            return_types: vec![Box::new(Type::String)],
        };

        // Different return type
        let different_return = Type::Function {
            params: vec![Box::new(Type::Number)],
            return_types: vec![Box::new(Type::Bool)], // String -> Bool
        };
        assert!(!base_func.is_assignable_to(&different_return));
        assert!(!different_return.is_assignable_to(&base_func));

        // Different number of return values
        let multiple_returns = Type::Function {
            params: vec![Box::new(Type::Number)],
            return_types: vec![Box::new(Type::String), Box::new(Type::Bool)],
        };
        assert!(!base_func.is_assignable_to(&multiple_returns));
        assert!(!multiple_returns.is_assignable_to(&base_func));

        // Void function vs non-void
        let void_func = Type::Function {
            params: vec![Box::new(Type::Number)],
            return_types: vec![],
        };
        assert!(!base_func.is_assignable_to(&void_func));
        assert!(!void_func.is_assignable_to(&base_func));
    }

    #[test]
    fn test_function_assignability_with_tuples() {
        // Function returning plain tuple
        let plain_tuple_func = Type::Function {
            params: vec![],
            return_types: vec![Box::new(Type::Tuple(vec![
                Box::new(Type::Number),
                Box::new(Type::String),
            ]))],
        };

        // Function returning named tuple with same structure
        let named_tuple_func = Type::Function {
            params: vec![],
            return_types: vec![Box::new(Type::NamedTuple(vec![
                ("result".to_string(), Box::new(Type::Number)),
                ("message".to_string(), Box::new(Type::String)),
            ]))],
        };

        // Should be assignable due to tuple/named tuple interchangeability
        assert!(plain_tuple_func.is_assignable_to(&named_tuple_func));
        assert!(named_tuple_func.is_assignable_to(&plain_tuple_func));
    }

    #[test]
    fn test_function_assignability_multiple_return_values() {
        // Function with multiple return values
        let multi_return = Type::Function {
            params: vec![Box::new(Type::Number), Box::new(Type::Number)],
            return_types: vec![
                Box::new(Type::Number),
                Box::new(Type::String),
                Box::new(Type::Bool),
            ],
        };

        // Same signature
        let same_multi_return = Type::Function {
            params: vec![Box::new(Type::Number), Box::new(Type::Number)],
            return_types: vec![
                Box::new(Type::Number),
                Box::new(Type::String),
                Box::new(Type::Bool),
            ],
        };

        assert!(multi_return.is_assignable_to(&same_multi_return));

        // Different order of return types
        let different_order_returns = Type::Function {
            params: vec![Box::new(Type::Number), Box::new(Type::Number)],
            return_types: vec![
                Box::new(Type::String),
                Box::new(Type::Number),
                Box::new(Type::Bool),
            ],
        };
        assert!(!multi_return.is_assignable_to(&different_order_returns));
    }

    #[test]
    fn test_function_assignability_with_nullable_parameters() {
        // Function with non-nullable parameter
        let non_nullable_param = Type::Function {
            params: vec![Box::new(Type::String)],
            return_types: vec![Box::new(Type::Number)],
        };

        // Function with nullable parameter
        let nullable_param = Type::Function {
            params: vec![Box::new(Type::Nullable(Box::new(Type::String)))],
            return_types: vec![Box::new(Type::Number)],
        };

        // This SHOULD work - more general function can substitute for more specific one
        assert!(nullable_param.is_assignable_to(&non_nullable_param));
    }

    #[test]
    fn test_function_assignability_with_nullable_returns() {
        // Function with non-nullable return
        let non_nullable_return = Type::Function {
            params: vec![Box::new(Type::Number)],
            return_types: vec![Box::new(Type::String)],
        };

        // Function with nullable return
        let nullable_return = Type::Function {
            params: vec![Box::new(Type::Number)],
            return_types: vec![Box::new(Type::Nullable(Box::new(Type::String)))],
        };

        assert!(non_nullable_return.is_assignable_to(&nullable_return));
        assert!(!nullable_return.is_assignable_to(&non_nullable_return));
    }

    #[test]
    fn test_void_functions() {
        // Void function (no return values)
        let void_func = Type::Function {
            params: vec![Box::new(Type::String)],
            return_types: vec![],
        };

        // Another void function with same parameters
        let another_void_func = Type::Function {
            params: vec![Box::new(Type::String)],
            return_types: vec![],
        };

        // Should be assignable
        assert!(void_func.is_assignable_to(&another_void_func));

        // Void function with different parameters
        let different_void_func = Type::Function {
            params: vec![Box::new(Type::Number)],
            return_types: vec![],
        };

        assert!(!void_func.is_assignable_to(&different_void_func));
    }

    #[test]
    fn test_complex_nested_function_types() {
        // Function that takes a function as parameter
        let higher_order_func = Type::Function {
            params: vec![Box::new(Type::Function {
                params: vec![Box::new(Type::Number)],
                return_types: vec![Box::new(Type::String)],
            })],
            return_types: vec![Box::new(Type::Bool)],
        };

        // Same higher-order function
        let same_higher_order = Type::Function {
            params: vec![Box::new(Type::Function {
                params: vec![Box::new(Type::Number)],
                return_types: vec![Box::new(Type::String)],
            })],
            return_types: vec![Box::new(Type::Bool)],
        };

        assert!(higher_order_func.is_assignable_to(&same_higher_order));

        // Different inner function signature
        let different_inner = Type::Function {
            params: vec![Box::new(Type::Function {
                params: vec![Box::new(Type::String)],
                return_types: vec![Box::new(Type::String)],
            })],
            return_types: vec![Box::new(Type::Bool)],
        };

        assert!(!higher_order_func.is_assignable_to(&different_inner));
    }

    // Helper to reset registry for each test
    fn setup_registry() {
        let mut registry = CLASS_REGISTRY.write().unwrap();
        registry.clear();

        registry.insert(
            "Animal".to_string(),
            ClassInfo {
                _name: "Animal".to_string(),
                superclass: None,
            },
        );
        registry.insert(
            "Dog".to_string(),
            ClassInfo {
                _name: "Dog".to_string(),
                superclass: Some("Animal".to_string()),
            },
        );
        registry.insert(
            "Cat".to_string(),
            ClassInfo {
                _name: "Cat".to_string(),
                superclass: Some("Animal".to_string()),
            },
        );
        registry.insert(
            "Poodle".to_string(),
            ClassInfo {
                _name: "Poodle".to_string(),
                superclass: Some("Dog".to_string()),
            },
        );
    }

    #[test]
    fn test_class_exact_match() {
        setup_registry();
        let a = Type::Class("Dog".to_string());
        let b = Type::Class("Dog".to_string());

        assert!(a.is_assignable_to(&b));
    }

    #[test]
    fn test_class_subtype() {
        setup_registry();
        let poodle = Type::Class("Poodle".to_string());
        let dog = Type::Class("Dog".to_string());
        let animal = Type::Class("Animal".to_string());

        assert!(poodle.is_assignable_to(&dog));
        assert!(poodle.is_assignable_to(&animal));
    }

    #[test]
    fn test_class_supertype_not_assignable_to_subtype() {
        setup_registry();
        let dog = Type::Class("Dog".to_string());
        let poodle = Type::Class("Poodle".to_string());

        assert!(!dog.is_assignable_to(&poodle));
    }

    #[test]
    fn test_class_unregistered() {
        setup_registry();
        let unknown = Type::Class("Dragon".to_string());
        let animal = Type::Class("Animal".to_string());

        // Unregistered class should not be assignable to anything
        assert!(!unknown.is_assignable_to(&animal));
    }
}
