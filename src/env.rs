use crate::function::{Function, Parameter};
use std::collections::HashMap;
use std::rc::Rc;
use crate::ops::OpCodes;
use crate::ast::Ptr;
use crate::types::{TypeCollector};

pub struct Env {
    pub types: TypeCollector,
    functions: HashMap<String, Rc<Function>>,
}

impl Env {
    pub fn new(types: TypeCollector) -> Env {
        //let types = TypeCollector::new();
        let unit = types.unit();
        let mut env = Env {
            types,
            functions: HashMap::<String, Rc<Function>>::new()
        };
        let mut println = Function::new("println".into(),
                                        unit,
                                        &Vec::<Ptr<Parameter>>::new());
        println.code.push(OpCodes::Println);
        println.code.push(OpCodes::Return);
        env.add_function("println".into(),
                         println);
        return env;
    }

    pub fn add_function(&mut self, name: String, function: Function) {
        self.functions.insert(name, Rc::new(function));
    }

    pub fn get_function(&self, name: String) -> Option<Rc<Function>> {
        return self.functions.get(name.as_str()).map(Rc::clone);
    }
}