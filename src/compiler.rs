use crate::ast::{Root, Expr, ExprKind, ExprKind::*, BinaryOp};
use crate::function::Function;
use crate::ops::OperationCodes;
use crate::values::{Value};
use std::convert::TryFrom;
use crate::env::Env;
use std::rc::Rc;

macro_rules! qm {
    /* NOTE: this is a temporary macro until I've rewritten this, also to make it easier to change the unmatched action */
    ($i:ident, $p:pat => $e:expr) => (
        match $i {
            $p => $e,
            _ => panic!("shouldn't happen tho"),
        }
    );
    ($i:ident, $( $p:pat => $e:expr ),+ $(,)? ) => ( // $(,)? is to always allow trailing commas
        match $i {
            $(
                $p => $e
            )+,
            _ => panic!("shouldn't happen tho"),
        }
    );
}

pub struct Compiler {
    env: Env,
}

impl Compiler {
    pub fn compile(ast: Root) -> Env {
        //println!("Compiling AST {:?}", ast);
        let mut compiler = Compiler {
            env: Env::new(),
        };
        compiler.compile_root(ast.functions);
        return compiler.env;
        // TODO: consider optimizing away unnecessary nodes
    }

    fn compile_root(&mut self, functions: Vec<Expr>) {
        for func in functions {
            //println!("compiling global func {:?}", func.kind);
            func.accept(self); // self is mutably borrowed here
        }
    }

    pub fn visit_function(&mut self, func: &ExprKind) {

        qm!(func,
            ExprKind::Function(name, return_type, parameters, block) => {
                let mut f = Function::new(name.clone(), return_type.clone(),
                                          parameters);
                //println!("Compiling function {}({:?}) -> {}", f.name, f.arguments, f.return_type);
                for p in &f.arguments {
                    f.code.push(OperationCodes::FunctionSetVar(p.0.clone()));
                }
                let b = &block.ptr;
                for e in &b.exprs {
                    e.accept_in_function(self, &mut f);
                    //println!("{:?}", e);
                }
                //println!("Code in function {}: {:?}", f.name.clone(), f.code);
                self.env.add_function(f.name.clone(), f);
                // TODO: add function to a list of compiled functions
            },
        );
    }

    pub fn visit_return(&mut self, ret: &ExprKind, func: &mut Function) {
        qm!(ret,
            ExprKind::Return(Some(expr)) => {
                let e = &expr.ptr;
                //println!("Compiling return with expr: {:?}", e);
                e.accept_in_function(self, func);
                func.code.push(OperationCodes::Return);
            },
            ExprKind::Return(None) => {
                //println!("Compiling empty return");
                func.code.push(OperationCodes::Return);
            }
        );
    }
    pub fn visit_binary_op(&mut self, bin_op: &ExprKind, func: &mut Function) {
        qm!(bin_op,
            ExprKind::BinaryOp(left, right, typ) => {
                //println!("Compiling binary op");
                (*left.ptr).accept_in_function(self, func);
                (*right.ptr).accept_in_function(self, func);
                match typ {
                    BinaryOp::Addition => {
                        //println!("Compiling add op");
                        func.code.push(OperationCodes::Add);
                    },
                    BinaryOp::Subtraction => {
                        unimplemented!();
                    },
                }
            });
    }

    pub fn visit_reference(&mut self, reference: &ExprKind, func: &mut Function) {
        qm!(reference,
            Reference(str) => {
                //println!("Compiling reference to {}", str);
                func.code.push(OperationCodes::Reference(str.clone()));
            });
    }

    pub fn visit_function_call(&mut self, fc: &ExprKind, func: &mut Function) {
        qm!(fc,
            FunctionCall(name, args) => {
                //println!("Compiling function call {}, {:?}", name, args);
                for a in args.iter().rev() {
                    (*a.ptr).accept_in_function(self, func);
                }
                func.code.push(OperationCodes::Push(Rc::new(Value::int(i64::try_from(args.len()).unwrap()))));
                func.code.push(OperationCodes::Push(Rc::new(Value::string(name.clone()))));
                func.code.push(OperationCodes::Call);
                // push args
                // push arg count
                // push name
                // call
            });
    }

    pub fn visit_int_constant(&mut self, i: &ExprKind, func: &mut Function) {
        qm!(i,
            IntConstant(num) => {
                func.code.push(OperationCodes::Push(Rc::new(Value::int(*num))));
        });
    }
}