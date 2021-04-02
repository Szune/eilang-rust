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
use crate::env::Env;
use crate::function::Function;
use crate::ops::OpCodes;
use crate::values::Value;
use std::convert::TryFrom;
use std::rc::Rc;

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
    pub fn compile(env: Env, ast: Root) -> Env {
        //println!("Compiling AST {:?}", ast);
        let mut compiler = Compiler { env };
        compiler.compile_root(ast.functions);
        compiler.env
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
                func.code
                    .push(OpCodes::Push(Rc::new(Value::string(ident.clone()))));
                func.code.push(OpCodes::DefVar)
            }
            _ => unreachable!(),
        }
    }

    pub fn visit_reassignment(&mut self, assign: &ExprKind, func: &mut Function) {
        match assign {
            ExprKind::Reassignment(ident, expr) => {
                expr.ptr.accept_in_function(self, func);
                func.code
                    .push(OpCodes::Push(Rc::new(Value::string(ident.clone()))));
                func.code.push(OpCodes::SetVar)
            }
            _ => unreachable!(),
        }
    }

    pub fn visit_if(&mut self, if_expr: &ExprKind, func: &mut Function) {
        match if_expr {
            ExprKind::If(if_expr, block) => {
                if_expr.ptr.accept_in_function(self, func);
                let op_addr = func.code.len();
                for e in &block.ptr.exprs {
                    e.accept_in_function(self, func);
                }

                let end_block_addr = func.code.len() + 1;
                // patch in branch op
                func.code
                    .insert(op_addr, OpCodes::BranchIfFalse(end_block_addr));
            }
            _ => unreachable!(),
        }
    }

    pub fn visit_if_else(&mut self, if_expr: &ExprKind, func: &mut Function) {
        match if_expr {
            ExprKind::IfElse(if_expr, true_block, else_block) => {
                if_expr.ptr.accept_in_function(self, func);
                let op_addr = func.code.len();
                for e in &true_block.ptr.exprs {
                    e.accept_in_function(self, func);
                }

                let true_block_end_jmp_addr = func.code.len() + 1; // + BranchIfFalse op

                let el_addr = func.code.len() + 2; // + BranchIfElse and Jump

                for e in &else_block.ptr.exprs {
                    e.accept_in_function(self, func);
                }

                // patch in branch op
                func.code.insert(op_addr, OpCodes::BranchIfFalse(el_addr));

                // patch in jump past else block at end of true block
                let end_block_addr = func.code.len() + 1;
                func.code
                    .insert(true_block_end_jmp_addr, OpCodes::Jump(end_block_addr));
            }
            _ => unreachable!(),
        }
    }

    pub fn visit_if_else_if(&mut self, if_expr: &ExprKind, func: &mut Function) {
        match if_expr {
            ExprKind::IfElseIf(if_expr, first_block, else_ifs) => {
                // 1. compile all if expressions
                if_expr.ptr.accept_in_function(self, func);
                func.code.push(OpCodes::BranchIfTrue(0)); // TODO: patch in the right address
                let first_if_op = func.code.len() - 1;

                let mut branch_op_indices = Vec::new();
                for else_if in else_ifs {
                    else_if.ptr.if_expr.ptr.accept_in_function(self, func);
                    func.code.push(OpCodes::BranchIfTrue(0)); // TODO: patch in the right address
                    branch_op_indices.push(func.code.len() - 1);
                }

                // 2. compile if block and patch branch op
                let first_if_block_jmp_to = func.code.len();
                *func.code.get_mut(first_if_op).unwrap() =
                    OpCodes::BranchIfTrue(first_if_block_jmp_to);
                for e in &first_block.ptr.exprs {
                    e.accept_in_function(self, func);
                }
                func.code.push(OpCodes::Jump(0));
                let first_if_block_end_jmp_op = func.code.len() - 1;

                let mut else_if_block_end_jmp_ops = Vec::new();
                // 3. compile all else if blocks and patch branch ops
                for (i, else_if) in else_ifs.iter().enumerate() {
                    let branch_op_index = branch_op_indices.get(i).unwrap();
                    let else_if_jmp_to = func.code.len();
                    *func.code.get_mut(*branch_op_index).unwrap() =
                        OpCodes::BranchIfTrue(else_if_jmp_to);

                    for e in &else_if.ptr.block.ptr.exprs {
                        e.accept_in_function(self, func);
                    }

                    func.code.push(OpCodes::Jump(0)); // TODO: patch in the right address
                    else_if_block_end_jmp_ops.push(func.code.len() - 1);
                }
                let end_pos = func.code.len();

                // 4. patch in the correct end address
                *func.code.get_mut(first_if_block_end_jmp_op).unwrap() = OpCodes::Jump(end_pos);

                for i in else_if_block_end_jmp_ops {
                    *func.code.get_mut(i).unwrap() = OpCodes::Jump(end_pos);
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn visit_if_else_if_else(&mut self, if_expr: &ExprKind, func: &mut Function) {
        match if_expr {
            ExprKind::IfElseIfElse(if_expr, first_block, else_ifs, else_block) => {
                // 1. compile all if expressions
                if_expr.ptr.accept_in_function(self, func);
                func.code.push(OpCodes::BranchIfTrue(0)); // TODO: patch in the right address
                let first_if_op = func.code.len() - 1;

                let mut branch_op_indices = Vec::new();
                for else_if in else_ifs {
                    else_if.ptr.if_expr.ptr.accept_in_function(self, func);
                    func.code.push(OpCodes::BranchIfTrue(0)); // TODO: patch in the right address
                    branch_op_indices.push(func.code.len() - 1);
                }
                func.code.push(OpCodes::Jump(0)); // jump unconditionally to else block
                let jmp_to_else_op = func.code.len() - 1;

                // 2. compile if block and patch branch op
                let first_if_block_jmp_to = func.code.len();
                *func.code.get_mut(first_if_op).unwrap() =
                    OpCodes::BranchIfTrue(first_if_block_jmp_to);
                for e in &first_block.ptr.exprs {
                    e.accept_in_function(self, func);
                }
                func.code.push(OpCodes::Jump(0));
                let first_if_block_end_jmp_op = func.code.len() - 1;

                let mut else_if_block_end_jmp_ops = Vec::new();
                // 3. compile all else if blocks and patch branch ops
                for (i, else_if) in else_ifs.iter().enumerate() {
                    let branch_op_index = branch_op_indices.get(i).unwrap();
                    let else_if_jmp_to = func.code.len();
                    *func.code.get_mut(*branch_op_index).unwrap() =
                        OpCodes::BranchIfTrue(else_if_jmp_to);

                    for e in &else_if.ptr.block.ptr.exprs {
                        e.accept_in_function(self, func);
                    }

                    func.code.push(OpCodes::Jump(0)); // TODO: patch in the right address
                    else_if_block_end_jmp_ops.push(func.code.len() - 1);
                }
                // 4. compile else block
                let else_block_start_addr = func.code.len();
                *func.code.get_mut(jmp_to_else_op).unwrap() = OpCodes::Jump(else_block_start_addr);
                for e in &else_block.ptr.exprs {
                    e.accept_in_function(self, func);
                }
                // else should not need a jump since it's at the very end

                // 5. patch in the correct end address
                let end_pos = func.code.len();

                *func.code.get_mut(first_if_block_end_jmp_op).unwrap() = OpCodes::Jump(end_pos);

                for i in else_if_block_end_jmp_ops {
                    *func.code.get_mut(i).unwrap() = OpCodes::Jump(end_pos);
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn visit_function(&mut self, func: &ExprKind) {
        match func {
            ExprKind::Function(new_func) => {
                let mut f = Function::new(
                    new_func.name.clone(),
                    new_func.return_type.clone(),
                    &new_func.parameters,
                );
                //println!("Compiling function {}({:?}) -> {}", f.name, f.arguments, f.return_type);
                for p in &f.parameters {
                    f.code.push(OpCodes::FunctionSetVar(p.name.clone()));
                }
                let b = &new_func.code.ptr;
                for e in &b.exprs {
                    e.accept_in_function(self, &mut f);
                    //println!("{:?}", e);
                }

                // if it's a function returning unit, write an implicit return
                if new_func.return_type.id == self.env.types.unit().id {
                    f.code.push(OpCodes::Push(Rc::new(Value::unit())));
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
                func.code.push(OpCodes::Push(Rc::new(Value::unit())));
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
                    BinaryOp::Multiplication => {
                        func.code.push(OpCodes::Multiply);
                    }
                    BinaryOp::Division => {
                        func.code.push(OpCodes::Divide);
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
                func.code.push(OpCodes::Push(Rc::new(Value::int(
                    i64::try_from(args.len()).unwrap(),
                ))));
                func.code
                    .push(OpCodes::Push(Rc::new(Value::string(name.clone()))));

                if self.env.is_builtin(name) {
                    func.code.push(OpCodes::CallRustFn);
                } else {
                    func.code.push(OpCodes::Call);
                }
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
            ExprKind::BoolConstant(boo) => {
                func.code.push(OpCodes::Push(Rc::new(Value::bool(*boo))))
            }
            _ => unreachable!(),
        }
    }

    pub fn visit_int_constant(&mut self, i: &ExprKind, func: &mut Function) {
        match i {
            ExprKind::IntConstant(num) => func.code.push(OpCodes::Push(Rc::new(Value::int(*num)))),
            _ => unreachable!(),
        }
    }

    pub fn visit_string_constant(&mut self, s: &ExprKind, func: &mut Function) {
        match s {
            ExprKind::StringConstant(str) => func
                .code
                .push(OpCodes::Push(Rc::new(Value::string(str.clone())))),
            _ => unreachable!(),
        }
    }

    pub fn visit_unit_constant(&mut self, _: &ExprKind, func: &mut Function) {
        func.code.push(OpCodes::Push(Rc::new(Value::unit())));
    }

    pub fn visit_pop(&self, _: &ExprKind, func: &mut Function) {
        func.code.push(OpCodes::Pop);
    }
}
