use crate::values::Value;
use std::rc::Rc;

#[derive(Debug)]
pub enum OpCodes {
    Println,
    Add,
    Subtract,
    Call,
    // comparison stuff
    BranchIfFalse(usize),
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanEquals,
    GreaterThanEquals,
    And,
    Or,
    Push(Rc<Value>),
    Reference(String),
    Return,
    SetVar,
    FunctionSetVar(String),
}