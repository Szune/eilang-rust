use crate::function::Function;
use std::collections::HashMap;
use std::rc::Rc;
use crate::ops::OpCodes;
use crate::ast::Ptr;

pub struct Env {
    functions: HashMap<String, Rc<Function>>,
}

impl Env {
    pub fn new() -> Env {
        let mut env = Env {
            functions: HashMap::<String, Rc<Function>>::new()
        };
        let mut println = Function::new("println".into(),
                                    "void".into(),
                                    &Vec::<Ptr<(String, String)>>::new());
        println.code.push(OpCodes::Println);
        println.code.push(OpCodes::Return);
        env.add_function("println".into(),
                         println);
        return env;
    }

    pub fn add_function(&mut self, name: String, function: Function) {
        self.functions.insert(name, Rc::new(function));
    }

    pub fn get_function(&self, name: String) -> Rc<Function> {
        return Rc::clone(self.functions.get(name.as_str()).unwrap());
    }
}