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
use crate::values::Value;
use std::ops::Deref;
use std::rc::Rc;
use std::cell::{Cell};
use std::collections::HashMap;
use std::borrow::{BorrowMut, Borrow};

pub struct Scope {
    /*r0: Cell<Rc<Value>>,
    r1: Cell<Rc<Value>>,
    r2: Cell<Rc<Value>>,*/
    vars: HashMap<String, Rc<Value>>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            /*r0: Cell::new(Value::empty().into()),
            r1: Cell::new(Value::empty().into()),
            r2: Cell::new(Value::empty().into()),*/
            vars: HashMap::<String, Rc<Value>>::new(),
        }
    }

    pub fn set_variable(&mut self, var: String, val: Rc<Value>) {
        self.vars.insert(var, val);
    }

    pub fn get_variable(&mut self, var: String) -> Rc<Value> {
        return Rc::clone(self.vars.borrow_mut().get(var.as_str()).unwrap());
    }
}

pub struct CallFrame {
    addr: Cell<usize>,
    func: Rc<Function>,
    called_with_arg_count: i64,
}

pub struct Interpreter {}

impl Interpreter {
    pub fn interpret(env: Env) {
        let main = env.get_function(".main".into()).expect("Missing a main function");
        let mut stack = Vec::<Rc<Value>>::new();
        let scopes = &mut Vec::<Scope>::new();
        scopes.push(Scope::new());
        let mut frames = Vec::<CallFrame>::new();
        let fra = CallFrame {
            addr: Cell::new(0usize),
            func: Rc::clone(&main),
            called_with_arg_count: 0,
        };
        frames.push(fra);

        let mut skip_inc = false;

        while !frames.is_empty() {
            let frame = &mut frames.last().unwrap();
            let op = frame.func.code.get(frame.addr.get())
                .expect(format!("Addr {} out of bounds, total opcodes {}", frame.addr.get(), frame.func.code.len()).as_str());
            //println!("on addr {}", frame.addr.get());
            match op {
                OpCodes::Add => {
                    Interpreter::op_code_add(&mut stack)
                }
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
                    let name = stack.pop();
                    let arg_count = match stack.pop().unwrap().deref() {
                        Value::Integer(num) => *num,
                        _ => panic!("noo"),
                    };
                    let name = match name.unwrap().deref() {
                        Value::String(s) => s.clone(),
                        _ => panic!("noo"),
                    };
                    let fr = CallFrame {
                        addr: Cell::new(0usize),
                        func: env.get_function(name.clone())
                            .expect(format!("Function {} not found", name.clone()).as_str()),
                        called_with_arg_count: arg_count,
                    };
                    scopes.push(Scope::new());
                    frames.push(fr);
                    skip_inc = true;
                    //println!("function call in interpreter: {:?}", n);
                    //println!("function call in interpreter: {:?} {:?}", n, args);
                }
                OpCodes::Push(value) => {
                    stack.push(Rc::clone(&value));
                }
                OpCodes::Reference(n) => {
                    let s = scopes.last_mut().unwrap();
                    let value = s.get_variable(n.clone());
                    stack.push(Rc::clone(&value));
                }
                OpCodes::Return => {
                    //println!("Returning from {}", frames.last().unwrap().func.name.clone());
                    frames.pop();
                    scopes.pop();
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
                OpCodes::Println => {
                    // argument count is popped in OperationCodes::Call, may need it later here though
                    if frame.called_with_arg_count < 1 {
                        println!();
                    } else {
                        // TODO: use called_with_arg_count and merge them to one print statement
                        // TODO: format the printed value depending on the value's type
                        let value = stack.pop().unwrap();
                        let print_value = get_printable_value(value);
                        println!("{}", print_value);
                    }
                }
                OpCodes::FunctionSetVar(n) => {
                    let value = stack.pop();
                    let s = scopes.last_mut().unwrap();
                    s.set_variable(n.clone(), value.unwrap());
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
            }
            if frames.is_empty() {
                break;
            }
            //println!("stack: {:?}", stack);
            if !skip_inc {
                frames.last().unwrap().addr.set(frames.last().unwrap().addr.get() + 1);
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

        stack.push(Rc::new(Value::Bool(result)));
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

        stack.push(Rc::new(Value::Bool(result)));
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

        stack.push(Rc::new(Value::Bool(result)));
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

        stack.push(Rc::new(Value::Bool(result)));
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

        stack.push(Rc::new(Value::Bool(result)));
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

        stack.push(Rc::new(Value::Bool(result)));
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
            }
            _ => unimplemented!(),
        };

        stack.push(Rc::new(Value::Bool(if not { !result } else { result })))
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
                    // this can't be the right way to do it...
                    stack.push(Rc::new(Value::string(l.clone() + format!("{}", &r.clone()).as_str())));
                }
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }
}

fn get_printable_value(value: Rc<Value>) -> String {
    match value.borrow() {
        Value::Unit => {
            "{}".into()
        }
        Value::Integer(i) => {
            format!("{}", i)
        }
        Value::Double(d) => {
            format!("{}", d)
        }
        Value::String(s) => {
            s.clone()
        }
        Value::Bool(b) => {
            b.to_string()
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
        main.code.push(OpCodes::Push(Rc::new(Value::String("Hello world".into()))));
        main.code.push(OpCodes::Push(Rc::new(Value::Integer(1))));
        main.code.push(OpCodes::Push(Rc::new(Value::String("println".into()))));
        main.code.push(OpCodes::Call);
        main.code.push(OpCodes::Return);
        let mut env = Env::new(types);
        env.add_function(".main".into(), main);
        let _ = Interpreter::interpret(env);
    }
}