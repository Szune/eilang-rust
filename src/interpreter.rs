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
    called_with_arg_count: i64
}

pub struct Interpreter {}
impl Interpreter {
    pub fn interpret(env: Env) {
        let main = env.get_function(".main".into());
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
            let op = frame.func.code.get(frame.addr.get()).unwrap();
            match op {
                OpCodes::Add => {
                    Interpreter::OpCodeAdd(&mut stack)
                },
                OpCodes::Subtract => {
                    let right = stack.pop();
                    let left = stack.pop();
                    match left.unwrap().deref() {
                        Value::Integer(l) => match right.unwrap().deref() {
                            Value::Integer(r) => {
                                stack.push(Rc::new(Value::int(l - r)));
                            },
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
                    let n = match name.unwrap().deref() {
                        Value::String(s) => s.clone(),
                        _ => panic!("noo"),
                    };
                    let fr = CallFrame {
                        addr: Cell::new(0usize),
                        func: env.get_function(n.clone()),
                        called_with_arg_count: arg_count,
                    };
                    scopes.push(Scope::new());
                    frames.push(fr);
                    skip_inc = true;
                    //println!("function call in interpreter: {:?}", n);
                    //println!("function call in interpreter: {:?} {:?}", n, args);
                },
                OpCodes::Push(value) => {
                    stack.push(Rc::clone(&value));
                },
                OpCodes::Reference(n) => {
                    let s = scopes.last_mut().unwrap();
                    let value = s.get_variable(n.clone());
                    stack.push(Rc::clone(&value));
                },
                OpCodes::Return => {
                    //println!("Returning from {}", frames.last().unwrap().func.name.clone());
                    frames.pop();
                    scopes.pop();
                },
                OpCodes::SetVar => {},
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
                },
                OpCodes::FunctionSetVar(n) => {
                    let value = stack.pop();
                    let s = scopes.last_mut().unwrap();
                    s.set_variable(n.clone(), value.unwrap());
                },
                OpCodes::BranchIfFalse(addr) => {
                    let brval = stack.pop().unwrap();
                    let brval = brval.deref();
                    if matches!(brval, Value::Bool(false)) {
                        frame.addr.set(*addr);
                    }
                }
                OpCodes::Equal => {
                    Interpreter::OpCodeEqual(&mut stack, false);
                }
                OpCodes::NotEqual => {
                    Interpreter::OpCodeEqual(&mut stack, true);
                }
                OpCodes::LessThan => {}
                OpCodes::GreaterThan => {}
                OpCodes::LessThanEquals => {}
                OpCodes::GreaterThanEquals => {}
                OpCodes::And => {}
                OpCodes::Or => {}
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
    }

    #[inline]
    fn OpCodeEqual(stack: &mut Vec<Rc<Value>>, not: bool) {
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
            _ => unimplemented!(),
        };

        stack.push(Rc::new(Value::Bool(if not { !result } else { result })))
    }

    #[inline]
    fn OpCodeAdd(stack: &mut Vec<Rc<Value>>) {
        let right = stack.pop().unwrap();
        let right = right.deref();
        let left = stack.pop().unwrap();
        let left = left.deref();
        match left {
            Value::Integer(l) => match right {
                Value::Integer(r) => {
                    stack.push(Rc::new(Value::int(l + r)));
                },
                _ => unimplemented!(),
            },
            Value::String(l) => match right {
                Value::String(r) => {
                    stack.push(Rc::new(Value::string(l.clone() + &r.clone())));
                },
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
        Value::Empty => {
            "{}".into()
        },
        Value::Integer(i) => {
            format!("{}", i)
        },
        Value::Double(d) => {
            format!("{}", d)
        },
        Value::String(s) => {
            s.clone()
        },
        Value::Bool(b) => {
            b.to_string()
        }
    }
}
