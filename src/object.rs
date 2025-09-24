use std::{cell::RefCell, rc::Rc};

/// Represents any reference type object that
/// is to be heap allocated
#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    String(String),
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
        }
    }

    /// Get the heap size of the object.
    pub fn size(&self) -> usize {
        let self_size = std::mem::size_of::<Self>();
        let obj_size = match &self.object {
            Object::String(s) => s.capacity(),
        };

        self_size + obj_size
    }
}
