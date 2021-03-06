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
use crate::function::Function;
use crate::ops::OpCodes;
use crate::scope::{Scope, ScopeType};
use crate::values::Value;
use std::cell::Cell;
use std::ops::Deref;
use std::rc::Rc;

pub struct CallFrame {
    addr: Cell<usize>,
    func: Rc<Function>,
    // might not need `called_with_arg_count`
    called_with_arg_count: i64,
}

pub struct Interpreter {}

impl Interpreter {
    pub fn interpret(env: Env) -> Rc<Value> {
        let main = env.get_function("main").expect("Missing a main function");
        let mut stack = Vec::<Rc<Value>>::new();
        let mut scope = Scope::new();
        let mut frames = vec![CallFrame {
            addr: Cell::new(0usize),
            func: Rc::clone(&main),
            called_with_arg_count: 0,
        }];

        let mut skip_inc = false;

        while !frames.is_empty() {
            let frame = &mut frames.last().unwrap();
            let op = frame.func.code.get(frame.addr.get()).unwrap_or_else(|| {
                panic!(
                    "Addr {} out of bounds, total opcodes {}",
                    frame.addr.get(),
                    frame.func.code.len()
                )
            });
            //println!("on addr {}", frame.addr.get());
            match op {
                OpCodes::Add => Interpreter::op_add(&mut stack),
                OpCodes::Subtract => Interpreter::op_sub(&mut stack),
                OpCodes::Multiply => Interpreter::op_mul(&mut stack),
                OpCodes::Divide => Interpreter::op_div(&mut stack),
                OpCodes::Call => {
                    let name = stack.pop().unwrap();
                    let arg_count = stack.pop().unwrap();
                    let name = match name.deref() {
                        Value::String(s) => s.clone(),
                        _ => panic!("Type error: expected string as function name in OpCodes::Call, found {:?}", name),
                    };
                    let arg_count = match arg_count.deref() {
                        Value::Integer(num) => *num,
                        v => panic!("Type error: expected integer as argument count in call to function {:?}, found {:?}", name, v),
                    };
                    let fr = CallFrame {
                        addr: Cell::new(0usize),
                        func: env
                            .get_function(&name)
                            .unwrap_or_else(|| panic!("Function {} not found", name)),
                        called_with_arg_count: arg_count,
                    };

                    scope = Scope::with_parent(scope);
                    frames.push(fr);
                    skip_inc = true;
                    //println!("function call in interpreter: {:?} {:?}", n, args);
                }
                OpCodes::CallRustFn => {
                    let name = stack.pop().unwrap();
                    let name = match name.deref() {
                        Value::String(s) => s.clone(),
                        _ => panic!("Type error: expected string as function name in OpCodes::Call, found {:?}", name),
                    };

                    env.call_rust_function(&name, &mut stack);
                }
                OpCodes::Push(value) => {
                    stack.push(Rc::clone(&value));
                }
                OpCodes::Reference(n) => {
                    let value = scope.get_variable(&n);
                    stack.push(Rc::clone(&value));
                }
                OpCodes::Return => {
                    if frame.func.name == "main" && scope.get_type() == ScopeType::Global {
                        // Could probably have a OpCodes::ReturnFromMain so that no regular returns are poisoned by this if statement
                        break;
                    }
                    frames.pop();
                    scope = scope.get_parent().unwrap();
                }
                OpCodes::DefVar => {
                    let ident = stack.pop().unwrap();
                    let value = stack.pop().unwrap();
                    let ident = match ident.deref() {
                        Value::String(s) => s,
                        _ => unreachable!(),
                    };
                    scope.define_variable(ident.clone(), value);
                }
                OpCodes::SetVar => {
                    let ident = stack.pop().unwrap();
                    let value = stack.pop().unwrap();
                    let ident = match ident.deref() {
                        Value::String(s) => s,
                        _ => unreachable!(),
                    };
                    scope.set_variable(ident.clone(), value);
                }
                OpCodes::FunctionSetVar(n) => {
                    let value = stack.pop();
                    scope.define_variable(n.clone(), value.unwrap());
                }
                OpCodes::Jump(addr) => {
                    frame.addr.set(*addr);
                    skip_inc = true;
                }
                OpCodes::BranchIfTrue(addr) => {
                    let br_val = stack.pop().unwrap();
                    let br_val = br_val.deref();
                    if matches!(br_val, Value::Bool(true)) {
                        frame.addr.set(*addr);
                        skip_inc = true;
                    }
                }
                OpCodes::BranchIfFalse(addr) => {
                    let br_val = stack.pop().unwrap();
                    let br_val = br_val.deref();
                    if matches!(br_val, Value::Bool(false)) {
                        frame.addr.set(*addr);
                        skip_inc = true;
                    }
                }
                OpCodes::Equal => {
                    Interpreter::op_equal(&mut stack, false);
                }
                OpCodes::NotEqual => {
                    Interpreter::op_equal(&mut stack, true);
                }
                OpCodes::LessThan => {
                    Interpreter::op_less_than(&mut stack);
                }
                OpCodes::GreaterThan => {
                    Interpreter::op_greater_than(&mut stack);
                }
                OpCodes::LessThanEquals => {
                    Interpreter::op_less_than_equals(&mut stack);
                }
                OpCodes::GreaterThanEquals => {
                    Interpreter::op_greater_than_equals(&mut stack);
                }
                OpCodes::And => {
                    Interpreter::op_and(&mut stack);
                }
                OpCodes::Or => {
                    Interpreter::op_or(&mut stack);
                }
                OpCodes::Pop => {
                    stack.pop();
                }
            }
            if frames.is_empty() {
                break;
            }
            //println!("stack: {:?}", stack);
            if !skip_inc {
                frames
                    .last()
                    .unwrap()
                    .addr
                    .set(frames.last().unwrap().addr.get() + 1);
            } else {
                skip_inc = false;
            }
        }

        assert!(!stack.is_empty());
        println!("Stack after interpreting: {:#?}", stack);
        Rc::clone(stack.last().unwrap())
    }

    #[inline]
    fn op_and(stack: &mut Vec<Rc<Value>>) {
        let right = stack.pop().unwrap();
        let right = right.deref();
        let left = stack.pop().unwrap();
        let left = left.deref();

        let result = match left {
            Value::Bool(l) => match right {
                Value::Bool(r) => *l && *r,
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        };

        stack.push(Rc::new(Value::bool(result)));
    }

    #[inline]
    fn op_or(stack: &mut Vec<Rc<Value>>) {
        let right = stack.pop().unwrap();
        let right = right.deref();
        let left = stack.pop().unwrap();
        let left = left.deref();

        let result = match left {
            Value::Bool(l) => match right {
                Value::Bool(r) => *l || *r,
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        };

        stack.push(Rc::new(Value::bool(result)));
    }

    #[inline]
    fn op_less_than(stack: &mut Vec<Rc<Value>>) {
        let right = stack.pop().unwrap();
        let right = right.deref();
        let left = stack.pop().unwrap();
        let left = left.deref();

        let result = match left {
            Value::Integer(l) => match right {
                Value::Integer(r) => l < r,
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        };

        stack.push(Rc::new(Value::bool(result)));
    }

    #[inline]
    fn op_greater_than(stack: &mut Vec<Rc<Value>>) {
        let right = stack.pop().unwrap();
        let right = right.deref();
        let left = stack.pop().unwrap();
        let left = left.deref();

        let result = match left {
            Value::Integer(l) => match right {
                Value::Integer(r) => l > r,
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        };

        stack.push(Rc::new(Value::bool(result)));
    }

    #[inline]
    fn op_less_than_equals(stack: &mut Vec<Rc<Value>>) {
        let right = stack.pop().unwrap();
        let right = right.deref();
        let left = stack.pop().unwrap();
        let left = left.deref();

        let result = match left {
            Value::Integer(l) => match right {
                Value::Integer(r) => l <= r,
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        };

        stack.push(Rc::new(Value::bool(result)));
    }

    #[inline]
    fn op_greater_than_equals(stack: &mut Vec<Rc<Value>>) {
        let right = stack.pop().unwrap();
        let right = right.deref();
        let left = stack.pop().unwrap();
        let left = left.deref();

        let result = match left {
            Value::Integer(l) => match right {
                Value::Integer(r) => l >= r,
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        };

        stack.push(Rc::new(Value::bool(result)));
    }

    #[inline]
    fn op_equal(stack: &mut Vec<Rc<Value>>, not: bool) {
        let right = stack.pop().unwrap();
        let right = right.deref();
        let left = stack.pop().unwrap();
        let left = left.deref();

        let result = match left {
            Value::Bool(l) => match right {
                Value::Bool(r) => l == r,
                _ => unimplemented!(),
            },
            Value::Integer(l) => match right {
                Value::Integer(r) => l == r,
                _ => unimplemented!(),
            },
            Value::String(l) => match right {
                Value::String(r) => l == r,
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        };

        stack.push(Rc::new(Value::bool(if not { !result } else { result })))
    }

    #[inline]
    fn op_add(stack: &mut Vec<Rc<Value>>) {
        let right = stack.pop().unwrap();
        let right = right.deref();
        let left = stack.pop().unwrap();
        let left = left.deref();
        match left {
            Value::Integer(l) => match right {
                Value::Integer(r) => {
                    stack.push(Rc::new(Value::int(l + r)));
                }
                _ => unimplemented!(),
            },
            Value::String(l) => match right {
                Value::String(r) => {
                    stack.push(Rc::new(Value::string(l.clone() + &r.clone())));
                }
                Value::Integer(r) => {
                    stack.push(Rc::new(Value::string(l.clone() + &r.to_string())));
                }
                Value::Unit => {
                    stack.push(Rc::new(Value::string(l.clone() + "()")));
                }
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }

    #[inline]
    fn op_sub(stack: &mut Vec<Rc<Value>>) {
        let right = stack.pop();
        let left = stack.pop();
        match left.unwrap().deref() {
            Value::Integer(l) => match right.unwrap().deref() {
                Value::Integer(r) => {
                    stack.push(Rc::new(Value::int(l - r)));
                }
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }

    #[inline]
    fn op_mul(stack: &mut Vec<Rc<Value>>) {
        let right = stack.pop();
        let left = stack.pop();
        match left.unwrap().deref() {
            Value::Integer(l) => match right.unwrap().deref() {
                Value::Integer(r) => {
                    stack.push(Rc::new(Value::int(l * r)));
                }
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }

    #[inline]
    fn op_div(stack: &mut Vec<Rc<Value>>) {
        let right = stack.pop();
        let left = stack.pop();
        match left.unwrap().deref() {
            Value::Integer(l) => match right.unwrap().deref() {
                Value::Integer(r) => {
                    stack.push(Rc::new(Value::int(l / r)));
                }
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::TypeCollector;

    #[test]
    pub fn test_println() {
        let types = TypeCollector::new();
        let mut main = Function::new("main".into(), types.any(), &Vec::new());
        main.code
            .push(OpCodes::Push(Rc::new(Value::string("Hello world".into()))));
        main.code.push(OpCodes::Push(Rc::new(Value::int(1))));
        main.code
            .push(OpCodes::Push(Rc::new(Value::string("println".into()))));
        main.code.push(OpCodes::CallRustFn);
        main.code.push(OpCodes::Return);
        let mut env = Env::new(types);
        crate::builtins::add(&mut env);
        env.add_function("main".into(), main);
        let _ = Interpreter::interpret(env);
    }
}
