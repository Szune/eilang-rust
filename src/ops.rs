use crate::values::Value;
use std::rc::Rc;

#[derive(Debug)]
pub enum OpCodes {
    Println,
    Add,
    Subtract,
    Call,
    Jump(usize),
    BranchIfFalse(usize),
    Push(Rc<Value>),
    Reference(String),
    Return,
    SetVar,
    FunctionSetVar(String),
    // comparison stuff
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanEquals,
    GreaterThanEquals,
    And,
    Or,
}