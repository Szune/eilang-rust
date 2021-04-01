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
use crate::values::Value;
use std::rc::Rc;

pub struct RustFunction {
    function: &'static dyn Fn(&mut Vec<Rc<Value>>) -> Rc<Value>,
}

impl RustFunction {
    pub fn new(func: &'static dyn Fn(&mut Vec<Rc<Value>>) -> Rc<Value>) -> RustFunction {
        RustFunction { function: func }
    }

    #[inline(always)]
    pub fn call(&self, stack: &mut Vec<Rc<Value>>) {
        let result = (self.function)(stack);
        stack.push(result);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_normal_call() {
        let testfn = RustFunction {
            function: &|stack| {
                let arg = stack.pop().unwrap();
                println!("Popped arg: {:?}", arg);
                Rc::new(Value::unit())
            },
        };

        let mut stack: Vec<Rc<Value>> = Vec::new();
        stack.push(Rc::new(Value::int(1)));
        testfn.call(&mut stack);
        stack.pop();
        stack.push(Rc::new(Value::string("hello test".into())));
        testfn.call(&mut stack);
        stack.pop();
        assert!(stack.is_empty());
    }
}
