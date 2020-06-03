use crate::values::Value;
use std::rc::Rc;

#[derive(Debug)]
pub enum OperationCodes {
    Println,
    Add,
    Subtract,
    Call,
    Push(Rc<Value>),
    Reference(String),
    Return,
    SetVar,
    FunctionSetVar(String),
}