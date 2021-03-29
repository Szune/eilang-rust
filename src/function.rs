use crate::ops::OpCodes;
use crate::ast::Ptr;
use std::ops::Deref;
use crate::types::Type;

#[derive(Debug,Clone)]
pub struct Parameter {
    pub name: String,
    pub typ: Type,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub return_type: Type,
    pub parameters: Vec<Parameter>,
    pub code: Vec<OpCodes>,
}

impl Function {
    pub fn new(name: String, return_type: Type, arguments: &Vec<Ptr<Parameter>>) -> Function {
        Function {
            name,
            return_type,
            parameters: arguments.iter()
                .map(|f| f.ptr.deref().clone())
                .collect(),
            code: Vec::new(),
        }
    }
}