use crate::ops::OperationCodes;
use crate::ast::Ptr;

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub return_type: String,
    pub arguments: Vec<(String,String)>,
    pub code: Vec<OperationCodes>,

}

impl Function {
    pub fn new(name: String, return_type: String, arguments: &Vec<Ptr<(String, String)>>) -> Function {
        // TODO: fix this huge mess
        let mut args : Vec<(String,String)> = Vec::new();
        for a in arguments {
            args.push((*a.ptr).clone());
        }
        Function {
            name,
            return_type,
            arguments: args,
            code: Vec::new(),
        }
    }
}