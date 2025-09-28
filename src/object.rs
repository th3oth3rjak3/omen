use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{Parameter, Statement},
    runtime::Environment,
    types::Type,
    value::Value,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub body: Statement,
    pub closure_env: Rc<RefCell<Environment>>, // Captured environment
}

/// Represents any reference type object that
/// is to be heap allocated
#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    String(String),
    Function(Function),
}

/// HeapObject is a wrapper type that contains metadata about
/// an object for garbage collection and memory management.
#[derive(Debug, Clone, PartialEq)]
pub struct HeapObject {
    /// When true, the object was reachable from GC roots
    /// meaning that the object should not be collected.
    pub marked: bool,
    /// Next is the next object in the intrusive list. When None,
    /// we've reached the end.
    pub next: Option<Rc<RefCell<HeapObject>>>,
    /// Object is the actual object contents stored in the wrapper.
    pub object: Object,
}

impl HeapObject {
    pub fn new(object: Object, next: Option<Rc<RefCell<HeapObject>>>) -> Self {
        Self {
            marked: false,
            next,
            object,
        }
    }

    /// Get a slice of all child objects that this object references.
    ///
    /// # Returns
    /// * A slice of objects referenced by this object.
    pub fn children(&self) -> Vec<Rc<RefCell<HeapObject>>> {
        match &self.object {
            Object::String(_) => vec![],
            Object::Function(fun) => {
                // Return any heap objects referenced in the closure environment
                // This will need to walk the environment and collect heap object references
                self.collect_env_references(&fun.closure_env)
            }
        }
    }

    fn collect_env_references(
        &self,
        env: &Rc<RefCell<Environment>>,
    ) -> Vec<Rc<RefCell<HeapObject>>> {
        let mut refs = vec![];
        let env_borrow = env.borrow();
        for scope in &env_borrow.scopes {
            let borrow_scope = scope.borrow();
            for value in borrow_scope.values() {
                if let Value::Object(obj) = value {
                    refs.push(obj.clone());
                }
            }
        }
        refs
    }

    /// Get the heap size of the object.
    pub fn size(&self) -> usize {
        let self_size = std::mem::size_of::<Self>();
        let obj_size = match &self.object {
            Object::String(s) => s.capacity(),
            Object::Function(_) => std::mem::size_of::<Function>(),
        };

        self_size + obj_size
    }
}
