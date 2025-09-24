use std::{cell::RefCell, rc::Rc};

use crate::object::HeapObject;

pub enum Value {
    Number(f64),
    Bool(bool),
    Nil,
    Object(Rc<RefCell<HeapObject>>),
}
