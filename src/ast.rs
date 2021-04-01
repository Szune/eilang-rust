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
use crate::compiler::Compiler;
use crate::function::{Function, Parameter};
use crate::types::Type;

#[derive(Debug, Clone)]
pub struct Ptr<T: ?Sized> {
    pub ptr: Box<T>,
}

#[allow(non_snake_case)]
pub fn Ptr<T>(val: T) -> Ptr<T> {
    // consider changing to a non-static lifetime of the AST
    Ptr { ptr: Box::new(val) }
}

#[derive(Debug)]
pub struct Root {
    pub functions: Vec<Expr>,
}

impl Root {
    pub fn new() -> Root {
        Root {
            functions: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct Block {
    pub exprs: Vec<Expr>,
}

impl Block {
    pub fn new() -> Block {
        Block { exprs: Vec::new() }
    }
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
}

impl Expr {
    pub fn new(kind: ExprKind) -> Expr {
        Expr { kind }
    }

    pub fn accept(&self, compiler: &mut Compiler) {
        match &self.kind {
            ExprKind::Function(_) => {
                //println!("visiting function {}", name.clone());
                compiler.visit_function(&self.kind);
            }
            _ => unreachable!(),
        }
    }

    pub fn accept_in_function(&self, compiler: &mut Compiler, f: &mut Function) {
        match &self.kind {
            ExprKind::Function(_) => {
                //println!("visiting function in function {}", name.clone());
                unimplemented!("function in function");
            }
            ExprKind::If(..) => {
                compiler.visit_if(&self.kind, f);
            }
            ExprKind::Comparison(..) => {
                compiler.visit_comparison(&self.kind, f);
            }
            ExprKind::BoolConstant(_) => {
                compiler.visit_bool_constant(&self.kind, f);
            }
            ExprKind::IntConstant(_) => {
                compiler.visit_int_constant(&self.kind, f);
            }
            ExprKind::StringConstant(_) => {
                compiler.visit_string_constant(&self.kind, f);
            }
            ExprKind::UnitConstant => {
                compiler.visit_unit_constant(&self.kind, f);
            }
            ExprKind::Return(_) => {
                //println!("visiting return in function");
                compiler.visit_return(&self.kind, f);
            }
            ExprKind::Reference(_) => {
                //println!("visiting reference in function");
                compiler.visit_reference(&self.kind, f);
            }
            ExprKind::BinaryOp(..) => {
                compiler.visit_binary_op(&self.kind, f);
            }
            ExprKind::FunctionCall(..) => {
                //println!("visiting function call in function");
                compiler.visit_function_call(&self.kind, f);
            }
            ExprKind::NewAssignment(..) => {
                compiler.visit_new_assignment(&self.kind, f);
            }
            ExprKind::Reassignment(..) => {
                compiler.visit_reassignment(&self.kind, f);
            }
            ExprKind::StackPop => {
                compiler.visit_pop(&self.kind, f);
            }
        }
    }
}

#[derive(Debug)]
pub struct FunctionExpr {
    pub name: String,
    pub return_type: Type,
    pub parameters: Vec<Ptr<Parameter>>,
    pub code: Ptr<Block>,
}

#[derive(Debug)]
pub enum ExprKind {
    Function(FunctionExpr),
    NewAssignment(String, Ptr<Expr>),
    Reassignment(String, Ptr<Expr>),
    If(Ptr<Expr>, Ptr<Block>, Option<Ptr<Block>>),
    IntConstant(i64),
    StringConstant(String),
    BoolConstant(bool),
    UnitConstant,
    Return(Option<Ptr<Expr>>),
    Reference(String),
    Comparison(Ptr<Expr>, Ptr<Expr>, Comparison),
    BinaryOp(Ptr<Expr>, Ptr<Expr>, BinaryOp),
    /// (ident, arguments)
    FunctionCall(String, Vec<Ptr<Expr>>),
    StackPop,
}

#[derive(Debug, PartialEq)]
pub enum Comparison {
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessThanEquals,
    GreaterThanEquals,
    And,
    Or,
}

#[derive(Debug)]
pub enum BinaryOp {
    Addition,
    Subtraction,
}
