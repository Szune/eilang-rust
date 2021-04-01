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
use crate::env::Env;
use crate::rustfn::RustFunction;
use crate::values::Value;
use std::borrow::Borrow;
use std::ops::Deref;
use std::rc::Rc;

fn get_printable_value(value: Rc<Value>) -> String {
    match value.borrow() {
        Value::Unit => "()".into(),
        Value::Integer(i) => {
            format!("{}", i)
        }
        Value::Double(d) => {
            format!("{}", d)
        }
        Value::String(s) => s.clone(),
        Value::Bool(b) => b.to_string(),
    }
}

fn add_println(env: &mut Env) {
    env.add_rust_function(
        "println".into(),
        RustFunction::new(&|stack| {
            let arg_count = stack.pop().unwrap();
            let arg_count = match arg_count.deref() {
                Value::Integer(num) => *num,
                v => panic!("Type error: expected integer as argument count in call to function println, found {:?}", v),
            };
            let mut values = Vec::new();

            for _ in 0..arg_count {
                values.push(stack.pop().unwrap());
            }

            if values.is_empty() {
                println!();
                return Rc::new(Value::unit());
            }

            let first_value = values.first().unwrap(); // TODO: rewrite to print all parts

            let print_value = get_printable_value(Rc::clone(first_value));

            println!(
                "{}",
                print_value
            );
            Rc::new(Value::unit())
        }),
    );
}

pub fn add(env: &mut Env) {
    add_println(env);
}
