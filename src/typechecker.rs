use crate::ast::{Root, ExprKind, Ptr, Expr, BinaryOp};
use std::cell::{RefCell, Cell};
use std::collections::HashMap;
use std::rc::Rc;

pub struct TypeChecker {}

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
    pub fn check(ast: &Root) -> TypeCheckerResult {
        let mut result = TypeCheckerResult::new();
        let types = build_types(ast);
        let functions = build_functions(ast, &types);
        // TODO: build_references() <- write a function that builds all the references made in a function
        // TODO: it should be run inside every loop of check_types that matches Function, and passed into that function
        // TODO: also - maybe let parser figure out if the referenced variable/whatever exists at the point of referencing it
        if !functions.0.success.get() {
            return functions.0;
        }
        result = check_types(result, ast, types, functions.1);
        return result;
    }
}

fn check_types(result: TypeCheckerResult, ast: &Root, types: HashMap<String, Rc<TCType>>, functions: HashMap<String, TCFunction>) -> TypeCheckerResult {
    for f in &ast.functions {
        match &f.kind {
            ExprKind::Function(name, _, _, code) => {
                let func = functions.get(name.as_str()).unwrap();
                println!("type checking function {}", name);
                let c = &(*code.ptr).exprs;
                for expr in c {
                    let expr_result = check_expr_types(expr, &types, &functions, func);
                    if !expr_result.success.get() {
                        result.success.set(false);
                        result.errors.borrow_mut().append(&mut expr_result.errors.borrow_mut());
                        return result;
                    }
                }
            }
            _ => panic!("this shouldn't happen, at least not until classes have been implemented"),
        }
    }
    result
}

fn check_expr_types(expr: &Expr, types: &HashMap<String, Rc<TCType>>, functions: &HashMap<String, TCFunction>, in_function: &TCFunction) -> TypeCheckerResult {
    let result = TypeCheckerResult::new();

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
            /*
            todo!("1. check if there's an expression, if so, type check it and go 2. otherwise go 3.\
                                2. check the return type of the function and compare it to the one in the expression,\
                                3. make sure that the function is of type void");
            */
            println!("return inside function {}", in_function.name);
            match ret_expr {
                Some(e) => {
                    if in_function.return_type.kind == TypeKind::BuiltIn_void {
                        result.success.set(false);
                        result.errors.borrow_mut().push(format!("Error: function '{}' has return type 'void' and cannot return values.", in_function.name));
                        return result;
                    } else {
                        // just get the type of the first expression
                        let expr_opt = get_first_expr_type(&(*e.ptr), types, functions, in_function);
                        match expr_opt.0 {
                            Some(err) => {
                                result.success.set(false);
                                result.errors.borrow_mut().push(format!("Error: return expression cannot work in function '{}': {}", in_function.name,  err));
                                return result;
                            },
                            _ => {},
                        }
                        let expr_type = expr_opt.1.unwrap();
                        if !check_type(&expr_type, &in_function.return_type) {
                            result.success.set(false);
                            result.errors.borrow_mut().push(format!("Error: function '{}' has return type '{}' and cannot return a value of type {}.", in_function.name, in_function.return_type.name, expr_type.name));
                            return result;
                        }
                    }
                }
                None => {
                    if in_function.return_type.kind != TypeKind::BuiltIn_void && in_function.name != ".main".to_string() {
                        // TODO: change error message
                        result.success.set(false);
                        result.errors.borrow_mut().push(format!("Error: function '{}' has return type '{}' and has to return a value.", in_function.name, in_function.return_type.name));
                        return result;
                    }
                }
            };
        }
        ExprKind::Reference(_) => {}
        ExprKind::BinaryOp(_, _, _) => {}
        ExprKind::FunctionCall(ident, args) => {
            for a in args {
                let ar = &(*a.ptr);
                if matches!(ar.kind, ExprKind::FunctionCall(_, _)) {
                    let println_arg_result = check_expr_types(ar, types, functions, in_function);
                    if !println_arg_result.success.get() {
                        result.success.set(false);
                        result.errors.borrow_mut().append(&mut println_arg_result.errors.borrow_mut());
                        return result;
                    }
                }
            }
            if ident == "println" {
                return result;
            } else {
                println!("type checking function call to {}", ident);
                let f = functions.get(ident.clone().as_str()).unwrap();
                if matches!(f.param_count, Some(_)) && f.param_count.unwrap() != args.len() {
                    result.success.set(false);
                    result.errors.borrow_mut().push(format!("Error: Wrong argument count in function call to '{}'", f.name));
                    return result;
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
                                result.success.set(false);
                                result.errors.borrow_mut().push(format!("Error: Wrong argument type '{}' for parameter '{}: {}' in call to function '{}'", "int", f.parameters[i].name, f.parameters[i].typ.name, f.name));
                                return result;
                            }
                        }
                        ExprKind::StringConstant(_) => {
                            let tctype = types.get("string").unwrap();
                            if !check_type(&tctype, &f.parameters[i].typ) {
                                result.success.set(false);
                                result.errors.borrow_mut().push(format!("Error: Wrong argument type '{}' for parameter '{}: {}' in call to function '{}'", "string", f.parameters[i].name, f.parameters[i].typ.name, f.name));
                                return result;
                            }
                        }
                        ExprKind::Return(_) => {}
                        /* This is type checking ARGUMENTS being used in a function call against the function declaration */
                        ExprKind::Reference(_) => {
                            todo!("1. check function parameter references,\
                            2. check variables when they are implemented");
                        }
                        ExprKind::BinaryOp(_, _, _) => {
                            todo!("1. get the result type of the binary expression (e.g. string + int = string),\
                            2. check type of arg");
                        }
                        ExprKind::FunctionCall(ident, _) => {
                            let func = functions.get(ident).unwrap();
                            if !check_type(&func.return_type, &f.parameters[i].typ) {
                                result.success.set(false);
                                result.errors.borrow_mut().push(format!("Error: Wrong argument type '{}' for parameter '{}: {}' in call to function '{}'", &func.return_type.name, f.parameters[i].name, f.parameters[i].typ.name, f.name));
                                return result;
                            }
                        }
                    }
                    i += 1;
                }
            }
        }
    };
    return result;
}

fn get_first_expr_type(expr: &Expr, types: &HashMap<String, Rc<TCType>>, functions: &HashMap<String, TCFunction>, in_function: &TCFunction) -> (Option<String>, Option<Rc<TCType>>) {
    let result = TypeCheckerResult::new();
    match &expr.kind {
        ExprKind::Function(name, _, _, _) => {
            // function definition isn't an expression (for now at least)
            return (Some("Function".to_string()), None);
            //return Rc::clone(types.get("void").unwrap());
        }
        ExprKind::If(_, _, _) => {
            // at least for now
            return (None, Some(Rc::clone(types.get("void").unwrap())));
        }
        ExprKind::IntConstant(_) => {
            return (None, Some(Rc::clone(types.get("int").unwrap())));
        }
        ExprKind::StringConstant(_) => {
            return (None, Some(Rc::clone(types.get("string").unwrap())));
        }
        ExprKind::Return(_) => {
            return (Some("Return".to_string()), None);
        }
        ExprKind::Reference(ident) => {
            for p in &in_function.parameters {
                if p.name == *ident {
                    return (None, Some(Rc::clone(&p.typ)));
                }
            }
            return (Some(format!("cannot reference {}", ident)), None);
        }
        ExprKind::BinaryOp(left, right, op) => {
            let l = get_first_expr_type(&(*left.ptr), types, functions, in_function);
            match &l.0 {
                Some(str) => {
                    return l;
                }
                _ => {},
            }
            let r = get_first_expr_type(&(*right.ptr), types, functions, in_function);
            match &r.0 {
                Some(str) => {
                    return r;
                }
                _ => {},
            }
            let binary_result_type = get_binary_op_result_type(l.1.unwrap(), r.1.unwrap(), op);
            return binary_result_type;
        }
        ExprKind::FunctionCall(name, _) => {
            let func = functions.get(name).unwrap();
            return (None, Some(Rc::clone(types.get(func.return_type.name.as_str()).unwrap())));
        }
    }
}

fn get_binary_op_result_type(left: Rc<TCType>, right: Rc<TCType>, op: &BinaryOp) -> (Option<String>, Option<Rc<TCType>>) {
    match op {
        BinaryOp::Addition => match &left.kind {
            TypeKind::BuiltIn_int => match &right.kind {
                TypeKind::BuiltIn_int => {
                    return (None, Some(Rc::clone(&left)));
                }
                TypeKind::BuiltIn_string => {
                    return (Some("int + string is not possible".to_string()), None);
                }
                TypeKind::BuiltIn_double => {
                    return (None, Some(Rc::clone(&right)));
                }
                _ => (Some(format!("int + {:?} is not possible", right.kind)), None),
            },
            TypeKind::BuiltIn_string => match &right.kind {
                TypeKind::BuiltIn_int => {
                    return (None, Some(Rc::clone(&left)));
                }
                TypeKind::BuiltIn_string => {
                    return (None, Some(Rc::clone(&left)));
                }
                TypeKind::BuiltIn_double => {
                    return (None, Some(Rc::clone(&left)));
                }
                _ => (Some(format!("string + {:?} is not possible", right.kind)), None),
            },
            TypeKind::BuiltIn_double => match &right.kind {
                TypeKind::BuiltIn_int => {
                    return (None, Some(Rc::clone(&left)));
                }
                TypeKind::BuiltIn_string => {
                    return (Some("double + string is not possible".to_string()), None);
                }
                TypeKind::BuiltIn_double => {
                    return (None, Some(Rc::clone(&left)));
                }
                _ => (Some(format!("double + {:?} is not possible", right.kind)), None),
            },
            TypeKind::BuiltIn_any => (Some(format!("any + {:?} is not possible", right.kind)), None),
            TypeKind::BuiltIn_void =>(Some(format!("void + {:?} is not possible", right.kind)), None),
            TypeKind::User_class => {
                return (Some(format!("user classes not implemented")), None);
            }
        },
        BinaryOp::Subtraction => (Some("subtraction not implemented".to_string()), None),
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

fn build_types(ast: &Root) -> HashMap<String, Rc<TCType>> {
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

fn build_functions(ast: &Root, types: &HashMap<String, Rc<TCType>>) -> (TypeCheckerResult, HashMap<String, TCFunction>) {
    let result = TypeCheckerResult::new();
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
                            result.success.set(false);
                            result.errors.borrow_mut().push(format!("Error: type '{}' does not exist -- found as parameter '{}' of function '{}'", cl_pa.1, cl_pa.0, ident));
                            return (result, functions);
                        }
                    }
                }
                let tc_return_type = types.get(return_type.as_str());
                let ret_type = match tc_return_type {
                    Some(t) => {
                        Rc::clone(t)
                    }
                    None => {
                        result.success.set(false);
                        result.errors.borrow_mut().push(format!("Error: type '{}' does not exist -- found as return type of function '{}'", return_type, ident));
                        return (result, functions);
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
    return (result, functions);
}
