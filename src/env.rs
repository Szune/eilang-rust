/*
 * eilang is an experimental programming language, this is its compiler and interpreter.
 * Copyright (C) 2021  Carl Erik Patrik Iwarson
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
use crate::ast::Ptr;
use crate::function::{Function, Parameter};
use crate::ops::OpCodes;
use crate::types::TypeCollector;
use std::collections::HashMap;
use std::rc::Rc;

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
            functions: HashMap::<String, Rc<Function>>::new(),
        };
        let mut println = Function::new("println".into(), unit, &Vec::<Ptr<Parameter>>::new());
        println.code.push(OpCodes::Println);
        println.code.push(OpCodes::Return);
        env.add_function("println".into(), println);
        return env;
    }

    pub fn add_function(&mut self, name: String, function: Function) {
        self.functions.insert(name, Rc::new(function));
    }

    pub fn get_function(&self, name: String) -> Option<Rc<Function>> {
        return self.functions.get(name.as_str()).map(Rc::clone);
    }
}
