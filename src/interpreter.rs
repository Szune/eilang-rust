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
use crate::scope::Scope;
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
    pub fn interpret(env: Env) {
        let main = env
            .get_function(".main".into())
            .expect("Missing a main function");
        let mut stack = Vec::<Rc<Value>>::new();
        let scopes = &mut Vec::<Scope>::new();
        scopes.push(Scope::new());
        let mut frames = Vec::<CallFrame>::new();
        frames.push(CallFrame {
            addr: Cell::new(0usize),
            func: Rc::clone(&main),
            called_with_arg_count: 0,
        });

        let mut skip_inc = false;

        while !frames.is_empty() {
            let frame = &mut frames.last().unwrap();
            let op = frame.func.code.get(frame.addr.get()).expect(
                format!(
                    "Addr {} out of bounds, total opcodes {}",
                    frame.addr.get(),
                    frame.func.code.len()
                )
                .as_str(),
            );
            //println!("on addr {}", frame.addr.get());
            match op {
                OpCodes::Add => Interpreter::op_code_add(&mut stack),
                OpCodes::Subtract => {
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
                            .expect(format!("Function {} not found", name).as_str()),
                        called_with_arg_count: arg_count,
                    };

                    // TODO: add current scope as parent
                    //  do I ever need access to multiple scopes from this function?
                    //  or could I just use a let mut scope = Scope::new();
                    //  and then do scope = Scope::with_parent(scope); <- that's not going to work, but you get the idea
                    //  we'll see!
                    //  Rc might be helpful
                    //let parent = scopes.last().unwrap();
                    //scopes.push(Scope::with_parent(parent));

                    scopes.push(Scope::new());
                    frames.push(fr);
                    skip_inc = true;
                    //println!("function call in interpreter: {:?}", n);
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
                    let s = scopes.last_mut().unwrap();
                    let value = s.get_variable(&n);
                    stack.push(Rc::clone(&value));
                }
                OpCodes::Return => {
                    //println!("Returning from {}", frames.last().unwrap().func.name.clone());
                    frames.pop();
                    scopes.pop();
                }
                OpCodes::DefVar => {
                    let ident = stack.pop().unwrap();
                    let value = stack.pop().unwrap();
                    let ident = match ident.deref() {
                        Value::String(s) => s,
                        _ => unreachable!(),
                    };
                    let s = scopes.last_mut().unwrap();
                    s.define_variable(ident.clone(), value);
                }
                OpCodes::SetVar => {
                    let ident = stack.pop().unwrap();
                    let value = stack.pop().unwrap();
                    let ident = match ident.deref() {
                        Value::String(s) => s,
                        _ => unreachable!(),
                    };
                    let s = scopes.last_mut().unwrap();
                    s.set_variable(ident.clone(), value);
                }
                OpCodes::FunctionSetVar(n) => {
                    let value = stack.pop();
                    let s = scopes.last_mut().unwrap();
                    s.define_variable(n.clone(), value.unwrap());
                }
                OpCodes::Jump(addr) => {
                    frame.addr.set(*addr);
                    skip_inc = true;
                }
                OpCodes::BranchIfFalse(addr) => {
                    let brval = stack.pop().unwrap();
                    let brval = brval.deref();
                    if matches!(brval, Value::Bool(false)) {
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

        println!("Stack after interpreting: {:#?}", stack);
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
    fn op_code_add(stack: &mut Vec<Rc<Value>>) {
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::TypeCollector;

    #[test]
    pub fn test_println() {
        let types = TypeCollector::new();
        let mut main = Function::new(".main".into(), types.any(), &Vec::new());
        main.code
            .push(OpCodes::Push(Rc::new(Value::string("Hello world".into()))));
        main.code.push(OpCodes::Push(Rc::new(Value::int(1))));
        main.code
            .push(OpCodes::Push(Rc::new(Value::string("println".into()))));
        main.code.push(OpCodes::CallRustFn);
        main.code.push(OpCodes::Return);
        let mut env = Env::new(types);
        crate::builtins::add(&mut env);
        env.add_function(".main".into(), main);
        let _ = Interpreter::interpret(env);
    }
}
