use crate::compiler::Compiler;
use crate::function::Function;

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
            kind
        }
    }

    pub fn accept(&self, compiler: &mut Compiler) {
        match &self.kind {
            ExprKind::Function(_, _, _, _) => {
                //println!("visiting function {}", name.clone());
                compiler.visit_function(&self.kind);
            },
            ExprKind::If(_, _, _) => {

            },
            ExprKind::IntConstant(_) => {

            },
            ExprKind::Return(_) => {

            },
            ExprKind::Reference(_) => {

            },
            ExprKind::BinaryOp(_, _, _) => {

            },
            ExprKind::FunctionCall(_, _) => {
                // rewrite to make every expression in the global scope be part of a single global function, like always?
                // i think yessu
                //println!("visiting function call in global scope");
            },
        }
    }

    pub fn accept_in_function(&self, compiler: &mut Compiler, f: &mut Function) {
        match &self.kind {
            ExprKind::Function(_, _, _, _) => {
                //println!("visiting function in function {}", name.clone());
                unimplemented!("function in function");
            },
            ExprKind::If(_, _, _) => {

            },
            ExprKind::IntConstant(_) => {
                //println!("int constant");
                compiler.visit_int_constant(&self.kind, f);
            },
            ExprKind::Return(_) => {
                //println!("visiting return in function");
                compiler.visit_return(&self.kind, f);
            },
            ExprKind::Reference(_) => {
                //println!("visiting reference in function");
                compiler.visit_reference(&self.kind, f);
            },
            ExprKind::BinaryOp(_, _, _) => {
                compiler.visit_binary_op(&self.kind, f);

            },
            ExprKind::FunctionCall(_, _) => {
                //println!("visiting function call in function");
                compiler.visit_function_call(&self.kind, f);
            },
        }
    }
}

#[derive(Debug)]
pub enum ExprKind {
    Function(String, String, Vec<Ptr<(String,String)>>, Ptr<Block>),
    If(Ptr<Expr>, Ptr<Block>, Option<Ptr<Expr>>),
    IntConstant(i64),
    Return(Option<Ptr<Expr>>),
    Reference(String),
    BinaryOp(Ptr<Expr>, Ptr<Expr>, BinaryOp),
    FunctionCall(String, Vec<Ptr<Expr>>),
}

#[derive(Debug)]
pub enum BinaryOp {
    Addition,
    Subtraction,
}