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
use crate::ast::*;
use crate::function::{Function};
use crate::ops::OpCodes;
use crate::values::{Value};
use std::convert::TryFrom;
use crate::env::Env;
use std::rc::Rc;
use crate::types::TypeCollector;

macro_rules! assert_returns (
    ($ops:expr, $name:expr) => {
        let mut returns = false;
        for op in &$ops {
            if matches!(op, OpCodes::Return) {
                returns = true;
                break;
            }
        }
        if !returns {
            panic!("Missing return in function {}", $name);
        }
    }
);

pub struct Compiler {
    env: Env,
}

impl Compiler {
    pub fn compile(ast: Root, types: TypeCollector) -> Env {
        //println!("Compiling AST {:?}", ast);
        let mut compiler = Compiler {
            env: Env::new(types),
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

    pub fn visit_comparison(&mut self, cmp: &ExprKind, func: &mut Function) {
        match cmp {
            ExprKind::Comparison(left, right, comparison) => {
                left.ptr.accept_in_function(self, func);
                right.ptr.accept_in_function(self, func);
                let op_code = match comparison {
                    Comparison::Equals => OpCodes::Equal,
                    Comparison::NotEquals => OpCodes::NotEqual,
                    Comparison::LessThan => OpCodes::LessThan,
                    Comparison::GreaterThan => OpCodes::GreaterThan,
                    Comparison::LessThanEquals => OpCodes::LessThanEquals,
                    Comparison::GreaterThanEquals => OpCodes::GreaterThanEquals,
                    Comparison::And => OpCodes::And,
                    Comparison::Or => OpCodes::Or,
                };
                func.code.push(op_code);
            }
            _ => unreachable!(),
        }
    }

    pub fn visit_new_assignment(&mut self, assign: &ExprKind, func: &mut Function) {
        match assign {
            ExprKind::NewAssignment(ident, expr) => {
                expr.ptr.accept_in_function(self, func);
                func.code.push(OpCodes::Push(Rc::new(Value::String(ident.clone()))));
                func.code.push(OpCodes::DefVar)
            }
            _ => unreachable!(),
        }
    }

    pub fn visit_reassignment(&mut self, assign: &ExprKind, func: &mut Function) {
        match assign {
            ExprKind::Reassignment(ident, expr) => {
                expr.ptr.accept_in_function(self, func);
                func.code.push(OpCodes::Push(Rc::new(Value::String(ident.clone()))));
                func.code.push(OpCodes::SetVar)
            }
            _ => unreachable!(),
        }
    }

    pub fn visit_if(&mut self, _if: &ExprKind, func: &mut Function) {
        match _if {
            ExprKind::If(if_expr, true_block, else_block) => {
                if let Some(el) = else_block {
                    if_expr.ptr.accept_in_function(self, func);
                    let op_addr = func.code.len();
                    for e in &true_block.ptr.exprs {
                        e.accept_in_function(self, func);
                    }

                    let true_block_end_jmp_addr = func.code.len() + 1; // + BranchIfFalse op

                    let el_addr = func.code.len() + 2; // + BranchIfElse and Jump

                    for e in &el.ptr.exprs {
                        e.accept_in_function(self, func);
                    }

                    // patch in branch op
                    func.code.insert(op_addr, OpCodes::BranchIfFalse(el_addr));

                    // patch in jump past else block at end of true block
                    let end_block_addr = func.code.len() + 1;
                    func.code.insert(true_block_end_jmp_addr, OpCodes::Jump(end_block_addr));
                } else {
                    if_expr.ptr.accept_in_function(self, func);
                    let op_addr = func.code.len();
                    for e in &true_block.ptr.exprs {
                        e.accept_in_function(self, func);
                    }

                    let end_block_addr = func.code.len() + 1;
                    // patch in branch op
                    func.code.insert(op_addr, OpCodes::BranchIfFalse(end_block_addr));
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn visit_function(&mut self, func: &ExprKind) {
        match func {
            ExprKind::Function(newf) => {
                let mut f = Function::new(newf.name.clone(), newf.return_type.clone(),
                                          &newf.parameters);
                //println!("Compiling function {}({:?}) -> {}", f.name, f.arguments, f.return_type);
                for p in &f.parameters {
                    f.code.push(OpCodes::FunctionSetVar(p.name.clone()));
                }
                let b = &newf.code.ptr;
                for e in &b.exprs {
                    e.accept_in_function(self, &mut f);
                    //println!("{:?}", e);
                }

                // if it's a function returning unit, write an implicit return
                if newf.return_type.id == self.env.types.unit().id {
                    f.code.push(OpCodes::Push(Rc::new(Value::Unit)));
                    f.code.push(OpCodes::Return);
                }

                assert_returns!(f.code, f.name);
                //println!("Code in function {}: {:?}", f.name.clone(), f.code);
                self.env.add_function(f.name.clone(), f);
            }
            _ => unreachable!(),
        }
    }

    pub fn visit_return(&mut self, ret: &ExprKind, func: &mut Function) {
        match ret {
            ExprKind::Return(Some(expr)) => {
                let e = &expr.ptr;
                //println!("Compiling return with expr: {:?}", e);
                e.accept_in_function(self, func);
                func.code.push(OpCodes::Return);
            }
            ExprKind::Return(None) => {
                //println!("Compiling empty return");
                func.code.push(OpCodes::Push(Rc::new(Value::Unit)));
                func.code.push(OpCodes::Return);
            }
            _ => unreachable!(),
        }
    }

    pub fn visit_binary_op(&mut self, bin_op: &ExprKind, func: &mut Function) {
        match bin_op {
            ExprKind::BinaryOp(left, right, typ) => {
                //println!("Compiling binary op");
                (*left.ptr).accept_in_function(self, func);
                (*right.ptr).accept_in_function(self, func);
                match typ {
                    BinaryOp::Addition => {
                        //println!("Compiling add op");
                        func.code.push(OpCodes::Add);
                    }
                    BinaryOp::Subtraction => {
                        func.code.push(OpCodes::Subtract);
                    }
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn visit_reference(&mut self, reference: &ExprKind, func: &mut Function) {
        match reference {
            ExprKind::Reference(str) => {
                //println!("Compiling reference to {}", str);
                func.code.push(OpCodes::Reference(str.clone()));
            }
            _ => unreachable!(),
        }
    }

    pub fn visit_function_call(&mut self, fc: &ExprKind, func: &mut Function) {
        match fc {
            ExprKind::FunctionCall(name, args) => {
                //println!("Compiling function call {}, {:?}", name, args);
                for a in args.iter().rev() {
                    (*a.ptr).accept_in_function(self, func);
                }
                func.code.push(OpCodes::Push(Rc::new(Value::int(i64::try_from(args.len()).unwrap()))));
                func.code.push(OpCodes::Push(Rc::new(Value::string(name.clone()))));
                func.code.push(OpCodes::Call);
                // push args
                // push arg count
                // push name
                // call
            }
            _ => unreachable!(),
        }
    }

    pub fn visit_bool_constant(&mut self, b: &ExprKind, func: &mut Function) {
        match b {
            ExprKind::BoolConstant(boo) =>
                func.code.push(OpCodes::Push(Rc::new(Value::bool(*boo)))),
            _ => unreachable!(),
        }
    }

    pub fn visit_int_constant(&mut self, i: &ExprKind, func: &mut Function) {
        match i {
            ExprKind::IntConstant(num) =>
                func.code.push(OpCodes::Push(Rc::new(Value::int(*num)))),
            _ => unreachable!(),
        }
    }


    pub fn visit_string_constant(&mut self, s: &ExprKind, func: &mut Function) {
        match s {
            ExprKind::StringConstant(str) =>
                func.code.push(OpCodes::Push(Rc::new(Value::string(str.clone())))),
            _ => unreachable!(),
        }
    }
}