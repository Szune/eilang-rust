use crate::ast::{Root, ExprKind, Ptr, Expr, BinaryOp};
use std::cell::{RefCell, Cell};
use std::collections::HashMap;
use std::rc::Rc;

pub struct TypeChecker {
    result: TypeCheckerResult,
}

#[derive(Debug, PartialEq)]
pub enum TypeKind {
    BuiltIn_int,
    BuiltIn_string,
    BuiltIn_double,
    BuiltIn_any,
    BuiltIn_void,
    User_class,
}

#[derive(Debug)]
pub struct MaybeTCType {
    typ: Option<Rc<TCType>>,
    error: Option<String>,
}

impl MaybeTCType {
    pub fn ok(typ: Rc<TCType>) -> MaybeTCType {
        MaybeTCType {
            typ: Some(typ),
            error: None,
        }
    }

    pub fn err(err: String) -> MaybeTCType {
        MaybeTCType {
            typ: None,
            error: Some(err),
        }
    }
}

#[derive(Debug)]
pub struct TCParameter {
    name: String,
    typ: Rc<TCType>,
}

#[derive(Debug)]
pub struct TCType {
    name: String,
    kind: TypeKind,
    base_type: Option<Ptr<TCFunction>>,
}

#[derive(Debug)]
pub struct TCFunction {
    name: String,
    return_type: Rc<TCType>,
    param_count: Option<usize>,
    // None = "infinite" parameter count
    parameters: Vec<TCParameter>,
}

pub struct TypeCheckerResult {
    pub success: Cell<bool>,
    pub errors: RefCell<Vec<String>>,
}

impl TypeCheckerResult {
    pub fn new() -> TypeCheckerResult {
        TypeCheckerResult {
            success: true.into(),
            errors: Vec::new().into(),
        }
    }
}

impl TypeChecker {
    fn new() -> TypeChecker {
        TypeChecker {
            result: TypeCheckerResult::new()
        }
    }
    pub fn check(ast: &Root) -> TypeCheckerResult {
        let mut type_checker = TypeChecker::new();
        let types = type_checker.build_types(ast);
        let functions = type_checker.build_functions(ast, &types);
        // TODO: build_references() <- write a function that builds all the references made in a function
        // TODO: it should be run inside every loop of check_types that matches Function, and passed into that function
        // TODO: also - maybe let parser figure out if the referenced variable/whatever exists at the point of referencing it
        if functions.is_none() {
            return type_checker.result;
        }
        type_checker.check_types(ast, types, functions.unwrap());
        return type_checker.result;
    }

    fn add_error(&mut self, err: String) {
        self.result.success.set(false);
        self.result.errors.borrow_mut().push(err);
    }

    fn add_errors(&mut self, errs: RefCell<Vec<String>>) {
        self.result.success.set(false);
        self.result.errors.borrow_mut().append(&mut errs.borrow_mut());
    }

    fn check_types(&mut self, ast: &Root, types: HashMap<String, Rc<TCType>>, functions: HashMap<String, TCFunction>) {
        for f in &ast.functions {
            match &f.kind {
                ExprKind::Function(name, _, _, code) => {
                    let func = functions.get(name.as_str()).unwrap();
                    println!("type checking function {}", name);
                    let c = &(*code.ptr).exprs;
                    for expr in c {
                        self.check_expr_types(expr, &types, &functions, func);
                    }
                }
                _ => panic!("this shouldn't happen, at least not until classes have been implemented"),
            }
        }
    }

    fn check_expr_types(&mut self, expr: &Expr, types: &HashMap<String, Rc<TCType>>, functions: &HashMap<String, TCFunction>, in_function: &TCFunction) {
        match &expr.kind {
            ExprKind::Function(_, _, _, _) => unimplemented!("nested functions"),
            ExprKind::If(_, _, _) => {}
            ExprKind::IntConstant(_) => {}
            ExprKind::StringConstant(_) => {}
            ExprKind::Return(ret_expr) => {
                // TODO: in the parser? make sure that there is always a return expr if the function isn't void
                // TODO: also make sure that there is a return expr in every if-else-branch
                // TODO: also make sure that if there is a return expr before an if-else-set,
                // TODO: it skips checking if the if-else-set has a return in it (because it won't be necessary)
                println!("return inside function {}", in_function.name);
                match ret_expr {
                    Some(e) => {
                        if in_function.return_type.kind == TypeKind::BuiltIn_void {
                            self.add_error(format!("Error: function '{}' has return type 'void' and cannot return values.", in_function.name));
                        } else {
                            // just get the type of the first expression
                            let expr_opt = self.get_first_expr_type(&(*e.ptr), types, functions, in_function);
                            if expr_opt.error.is_some() {
                                self.add_error(format!("Error: return expression cannot work in function '{}': {}", in_function.name, expr_opt.error.unwrap()));
                            } else {
                                let expr_type = expr_opt.typ.unwrap();
                                if !check_type(&expr_type, &in_function.return_type) {
                                    self.add_error(format!("Error: function '{}' has return type '{}' and cannot return a value of type {}.", in_function.name, in_function.return_type.name, expr_type.name));
                                }
                            }
                        }
                    }
                    None => {
                        if in_function.return_type.kind != TypeKind::BuiltIn_void && in_function.name != ".main".to_string() {
                            self.add_error(format!("Error: function '{}' has return type '{}' and has to return a value.", in_function.name, in_function.return_type.name));
                        }
                    }
                };
            }
            ExprKind::Reference(_) => {}
            ExprKind::BinaryOp(_, _, _) => {}
            ExprKind::FunctionCall(ident, args) => self.type_check_function_call(types, functions, in_function, ident, args)
        };
    }

    fn type_check_function_call(&mut self, types: &HashMap<String, Rc<TCType>>, functions: &HashMap<String, TCFunction>, in_function: &TCFunction, ident: &String, args: &Vec<Ptr<Expr>>) {
        for a in args {
            let ar = &(*a.ptr);
            if matches!(ar.kind, ExprKind::FunctionCall(_, _)) {
                self.check_expr_types(ar, types, functions, in_function);
            }
        }
        if ident != "println" {
            println!("type checking function call to {}", ident);
            let f = functions.get(ident.clone().as_str()).unwrap();
            if matches!(f.param_count, Some(_)) && f.param_count.unwrap() != args.len() {
                self.add_error(format!("Error: Wrong argument count in function call to '{}'", f.name));
                // don't check argument types if enough aren't supplied, change this if it's more annoying than useful
                return;
            }
            let mut i = 0;
            for a in args {
                let ar = &(*a.ptr);
                match &ar.kind {
                    /* This is type checking ARGUMENTS being used in a function call against the function declaration */
                    ExprKind::Function(_, _, _, _) => {}
                    ExprKind::If(_, _, _) => {}
                    ExprKind::IntConstant(_) => {
                        // TODO: store more metadata to show better error messages, like line number and the actual text on the line, highlighting the span where the error happens
                        let tctype = types.get("int").unwrap();
                        if !check_type(&tctype, &f.parameters[i].typ) {
                            self.add_error(format!("Error: Wrong argument type '{}' for parameter '{}: {}' in call to function '{}'", tctype.name, f.parameters[i].name, f.parameters[i].typ.name, f.name));
                        }
                    }
                    ExprKind::StringConstant(_) => {
                        let tctype = types.get("string").unwrap();
                        if !check_type(&tctype, &f.parameters[i].typ) {
                            self.add_error(format!("Error: Wrong argument type '{}' for parameter '{}: {}' in call to function '{}'", tctype.name, f.parameters[i].name, f.parameters[i].typ.name, f.name));
                        }
                    }
                    ExprKind::Return(_) => {}
                    /* This is type checking ARGUMENTS being used in a function call against the function declaration */
                    ExprKind::Reference(_) => {
                        todo!("1. check function parameter references,\
                            2. check variables when they are implemented");
                    }
                    ExprKind::BinaryOp(left, right, op) => {
                        let res_type = self.get_binary_op_result_type(left, right, op, types, functions, in_function);
                        if res_type.error.is_some() {
                            self.add_error(format!("Error: binary op expression cannot work for parameter '{}: {}' of function '{}': {}", f.parameters[i].name, f.parameters[i].typ.name, f.name, res_type.error.unwrap()));
                        }
                        else {
                            let expr_type = res_type.typ.unwrap();
                            if !check_type(&expr_type, &f.parameters[i].typ) {
                                self.add_error(format!("Error: Wrong argument type '{}' for parameter '{}: {}' in call to function '{}'", expr_type.name, f.parameters[i].name, f.parameters[i].typ.name, f.name));
                            }
                        }
                    },
                    ExprKind::FunctionCall(ident, _) => {
                        let func = functions.get(ident).unwrap();
                        if !check_type(&func.return_type, &f.parameters[i].typ) {
                            self.add_error(format!("Error: Wrong argument type '{}' for parameter '{}: {}' in call to function '{}'", &func.return_type.name, f.parameters[i].name, f.parameters[i].typ.name, f.name));
                        }
                    }
                }
                i += 1;
            }
        }
    }

    fn get_first_expr_type(&mut self, expr: &Expr, types: &HashMap<String, Rc<TCType>>, functions: &HashMap<String, TCFunction>, in_function: &TCFunction) -> MaybeTCType {
        match &expr.kind {
            ExprKind::Function(name, _, _, _) => {
                // function definition isn't an expression (for now at least)
                return MaybeTCType::err("Function".to_string())
                //return Rc::clone(types.get("void").unwrap());
            }
            ExprKind::If(_, _, _) => {
                // at least for now
                return MaybeTCType::ok(Rc::clone(types.get("void").unwrap()));
            }
            ExprKind::IntConstant(_) => {
                return MaybeTCType::ok(Rc::clone(types.get("int").unwrap()));
            }
            ExprKind::StringConstant(_) => {
                return MaybeTCType::ok(Rc::clone(types.get("string").unwrap()));
            }
            ExprKind::Return(_) => {
                return MaybeTCType::err("Return".to_string());
            }
            ExprKind::Reference(ident) => {
                for p in &in_function.parameters {
                    if p.name == *ident {
                        return MaybeTCType::ok(Rc::clone(&p.typ));
                    }
                }
                return MaybeTCType::err(format!("cannot reference {} (reference not found)", ident));
            }
            ExprKind::BinaryOp(left, right, op) =>
                return self.get_binary_op_result_type(left, right, op, types, functions, in_function),
            ExprKind::FunctionCall(name, _) => {
                let func = functions.get(name);
                return if func.is_none() {
                    MaybeTCType::err(format!("Function '{}' could not be found", name))
                } else {
                    MaybeTCType::ok(Rc::clone(types.get(func.unwrap().return_type.name.as_str()).unwrap()))
                }
            }
        }
    }

    fn get_binary_op_result_type(&mut self, l: &Ptr<Expr>, r: &Ptr<Expr>, op: &BinaryOp, types: &HashMap<String, Rc<TCType>>, functions: &HashMap<String, TCFunction>, in_function: &TCFunction) -> MaybeTCType {
        let le = self.get_first_expr_type(&(*l.ptr), types, functions, in_function);
        if le.error.is_some() {
            return le; // if there is an error at this point, return
        }
        let ri = self.get_first_expr_type(&(*r.ptr), types, functions, in_function);
        if ri.error.is_some() {
            return ri;
        }
        let left = le.typ.unwrap();
        let right = ri.typ.unwrap();
        match op {
            BinaryOp::Addition => match &left.kind {
                TypeKind::BuiltIn_int => match &right.kind {
                    TypeKind::BuiltIn_int => {
                        return MaybeTCType::ok( Rc::clone(&left));
                    }
                    TypeKind::BuiltIn_string => {
                        return MaybeTCType::err("int + string is not possible".to_string());
                    }
                    TypeKind::BuiltIn_double => {
                        return MaybeTCType::ok(Rc::clone(&right));
                    }
                    _ => MaybeTCType::err(format!("int + {:?} is not possible", right.kind)),
                },
                TypeKind::BuiltIn_string => match &right.kind {
                    TypeKind::BuiltIn_int => {
                        return MaybeTCType::ok(Rc::clone(&left));
                    }
                    TypeKind::BuiltIn_string => {
                        return MaybeTCType::ok(Rc::clone(&left));
                    }
                    TypeKind::BuiltIn_double => {
                        return MaybeTCType::ok(Rc::clone(&left));
                    }
                    _ => MaybeTCType::err(format!("string + {:?} is not possible", right.kind)),
                },
                TypeKind::BuiltIn_double => match &right.kind {
                    TypeKind::BuiltIn_int => {
                        return MaybeTCType::ok(Rc::clone(&left));
                    }
                    TypeKind::BuiltIn_string => {
                        return MaybeTCType::err("double + string is not possible".to_string());
                    }
                    TypeKind::BuiltIn_double => {
                        return MaybeTCType::ok(Rc::clone(&left));
                    }
                    _ => MaybeTCType::err(format!("double + {:?} is not possible", right.kind)),
                },
                TypeKind::BuiltIn_any => MaybeTCType::err(format!("any + {:?} is not possible", right.kind)),
                TypeKind::BuiltIn_void => MaybeTCType::err(format!("void + {:?} is not possible", right.kind)),
                TypeKind::User_class => {
                    return MaybeTCType::err(format!("user classes not implemented"));
                }
            },
            BinaryOp::Subtraction => match &left.kind {
                TypeKind::BuiltIn_int => match &right.kind {
                    TypeKind::BuiltIn_int => {
                        return MaybeTCType::ok( Rc::clone(&left));
                    }
                    TypeKind::BuiltIn_string => {
                        return MaybeTCType::err("int - string is not possible".to_string());
                    }
                    TypeKind::BuiltIn_double => {
                        return MaybeTCType::ok(Rc::clone(&right));
                    }
                    _ => MaybeTCType::err(format!("int - {:?} is not possible", right.kind)),
                },
                TypeKind::BuiltIn_double => match &right.kind {
                    TypeKind::BuiltIn_int => {
                        return MaybeTCType::ok(Rc::clone(&left));
                    }
                    TypeKind::BuiltIn_string => {
                        return MaybeTCType::err("double - string is not possible".to_string());
                    }
                    TypeKind::BuiltIn_double => {
                        return MaybeTCType::ok(Rc::clone(&left));
                    }
                    _ => MaybeTCType::err(format!("double - {:?} is not possible", right.kind)),
                },
                TypeKind::BuiltIn_string => MaybeTCType::err(format!("string - {:?} is not possible", right.kind)),
                TypeKind::BuiltIn_any => MaybeTCType::err(format!("any - {:?} is not possible", right.kind)),
                TypeKind::BuiltIn_void => MaybeTCType::err(format!("void - {:?} is not possible", right.kind)),
                TypeKind::User_class => {
                    return MaybeTCType::err(format!("user classes not implemented"));
                }
            },
        }
    }

    fn build_types(&mut self, ast: &Root) -> HashMap<String, Rc<TCType>> {
        let mut types = HashMap::new();
        types.insert("any".into(), Rc::new(TCType {
            name: "any".into(),
            kind: TypeKind::BuiltIn_any,
            base_type: None,
        }));
        types.insert("string".into(), Rc::new(TCType {
            name: "string".into(),
            kind: TypeKind::BuiltIn_string,
            base_type: None,
        }));
        types.insert("int".into(), Rc::new(TCType {
            name: "int".into(),
            kind: TypeKind::BuiltIn_int,
            base_type: None,
        }));
        types.insert("double".into(), Rc::new(TCType {
            name: "double".into(),
            kind: TypeKind::BuiltIn_double,
            base_type: None,
        }));
        types.insert("void".into(), Rc::new(TCType {
            name: "void".into(),
            kind: TypeKind::BuiltIn_void,
            base_type: None,
        }));
        return types;
    }


    fn build_functions(&mut self, ast: &Root, types: &HashMap<String, Rc<TCType>>) -> Option<HashMap<String, TCFunction>> {
        let mut functions = HashMap::new();
        for f in &ast.functions {
            match &f.kind {
                // currently there are no nested functions nor classes, so all functions can be found in the global scope
                ExprKind::Function(ident, return_type, parameters, _) => {
                    let mut params = Vec::<TCParameter>::new();
                    for p in parameters {
                        let cl_pa = (*p.ptr).clone();
                        let tctype = types.get(cl_pa.1.as_str());
                        match tctype {
                            Some(t) => {
                                params.push(TCParameter {
                                    name: cl_pa.0,
                                    typ: Rc::clone(t),
                                });
                            }
                            None => {
                                self.add_error(
                                    format!("Error: type '{}' does not exist -- found as parameter '{}' of function '{}'", cl_pa.1, cl_pa.0, ident));
                                return None;
                            }
                        }
                    }
                    let tc_return_type = types.get(return_type.as_str());
                    let ret_type = match tc_return_type {
                        Some(t) => {
                            Rc::clone(t)
                        }
                        None => {
                            self.add_error(
                                format!("Error: type '{}' does not exist -- found as return type of function '{}'", return_type, ident));
                            return None;
                        }
                    };
                    functions.insert(ident.clone(), TCFunction {
                        name: ident.clone(),
                        return_type: ret_type,
                        param_count: Some(parameters.len()),
                        parameters: params,
                    });
                }
                _ => unimplemented!(),
            };
        }
        return Some(functions);
    }
}


fn check_type(actual: &TCType, expected: &TCType) -> bool {
    if expected.kind == TypeKind::BuiltIn_any {
        return true;
    }
    if expected.name == actual.name {
        return true;
    }
    return false;
}

fn type_exists(typ: String, types: &HashMap<String, Rc<TCType>>) -> bool {
    return types.contains_key(typ.as_str());
}

