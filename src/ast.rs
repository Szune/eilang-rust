use crate::compiler::Compiler;
use crate::function::{Function, Parameter};
use crate::types::Type;

#[derive(Debug, Clone)]
pub struct Ptr<T: ?Sized> {
    pub ptr: Box<T>,
}

#[allow(non_snake_case)]
pub fn Ptr<T>(val: T) -> Ptr<T> { // consider changing to a non-static lifetime of the AST
    Ptr {
        ptr: Box::new(val)
    }
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
        Block {
            exprs: Vec::new(), // TODO: look up if you should set a capacity on Vec::new() or not
        }
    }
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
}

impl Expr {
    pub fn new(kind: ExprKind) -> Expr {
        Expr {
            kind,
        }
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
            ExprKind::If(_, _, _) => {
                compiler.visit_if(&self.kind, f);
            }
            ExprKind::Comparison(_, _, _) => {
                compiler.visit_comparison(&self.kind, f);
            }
            ExprKind::IntConstant(_) => {
                //println!("int constant");
                compiler.visit_int_constant(&self.kind, f);
            }
            ExprKind::StringConstant(_) => {
                //println!("int constant");
                compiler.visit_string_constant(&self.kind, f);
            }
            ExprKind::Return(_) => {
                //println!("visiting return in function");
                compiler.visit_return(&self.kind, f);
            }
            ExprKind::Reference(_) => {
                //println!("visiting reference in function");
                compiler.visit_reference(&self.kind, f);
            }
            ExprKind::BinaryOp(_, _, _) => {
                compiler.visit_binary_op(&self.kind, f);
            }
            ExprKind::FunctionCall(_, _) => {
                //println!("visiting function call in function");
                compiler.visit_function_call(&self.kind, f);
            }
            ExprKind::NewAssignment(_, _) => {
                compiler.visit_new_assignment(&self.kind, f);
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
    If(Ptr<Expr>, Ptr<Block>, Option<Ptr<Block>>),
    IntConstant(i64),
    StringConstant(String),
    Return(Option<Ptr<Expr>>),
    Reference(String),
    Comparison(Ptr<Expr>, Ptr<Expr>, Comparison),
    BinaryOp(Ptr<Expr>, Ptr<Expr>, BinaryOp),
    FunctionCall(String, Vec<Ptr<Expr>>),
}

#[derive(Debug,PartialEq)]
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