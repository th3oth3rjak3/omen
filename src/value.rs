use std::{cell::RefCell, rc::Rc};

use crate::object::HeapObject;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Nil,
    String(String),
    Object(Rc<RefCell<HeapObject>>),
}
