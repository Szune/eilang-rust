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
use crate::function::Function;
use crate::rustfn::RustFunction;
use crate::types::TypeCollector;
use crate::values::Value;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Env {
    pub types: TypeCollector,
    functions: HashMap<String, Rc<Function>>,
    rust_functions: HashMap<String, RustFunction>,
}

impl Env {
    pub fn new(types: TypeCollector) -> Env {
        let env = Env {
            types,
            functions: HashMap::new(),
            rust_functions: HashMap::new(),
        };
        return env;
    }

    pub fn add_function(&mut self, name: String, function: Function) {
        self.functions.insert(name, Rc::new(function));
    }

    pub fn get_function(&self, name: &str) -> Option<Rc<Function>> {
        return self.functions.get(name).map(Rc::clone);
    }

    pub fn add_rust_function(&mut self, name: String, function: RustFunction) {
        self.rust_functions.insert(name, function);
    }

    pub fn call_rust_function(&self, name: &str, stack: &mut Vec<Rc<Value>>) {
        self.rust_functions
            .get(name)
            .expect(&format!("Could not find rust function {}", name))
            .call(stack);
    }

    pub fn is_builtin(&self, name: &str) -> bool {
        self.rust_functions.contains_key(name)
    }
}
