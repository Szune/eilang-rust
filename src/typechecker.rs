use crate::ast::{Root, ExprKind, Ptr, Expr};
use std::cell::{RefCell, Cell};
use std::collections::HashMap;
use std::rc::Rc;

pub struct TypeChecker {
}

#[derive(Debug)]
pub struct TCParameter {
    name: String,
    typ: Rc<TCType>,
}

#[derive(Debug)]
pub struct TCType {
    name: String,
    base_type: Option<Ptr<TCFunction>>,
}

#[derive(Debug)]
pub struct TCFunction {
    name: String,
    return_type: Rc<TCType>,
    param_count: Option<usize>, // None = "infinite" parameter count
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
                println!("type checking function {}", name);
                let c = &(*code.ptr).exprs;
                for expr in c {
                    let expr_result = check_expr_types(expr, &types, &functions);
                    if !expr_result.success.get() {
                        result.success.set(false);
                        result.errors.borrow_mut().append(&mut expr_result.errors.borrow_mut());
                        return result;
                    }
                }
            },
            _ => panic!("this shouldn't happen, at least not until classes have been implemented"),
        }

    }
    result
}

fn check_expr_types(expr: &Expr, types: &HashMap<String, Rc<TCType>>, functions: &HashMap<String, TCFunction>) -> TypeCheckerResult {
    let result = TypeCheckerResult::new();

    match &expr.kind {
        ExprKind::Function(_, _, _, _) => unimplemented!("nested functions"),
        ExprKind::If(_, _, _) => {},
        ExprKind::IntConstant(_) => {},
        ExprKind::StringConstant(_) => {},
        ExprKind::Return(_) => {},
        ExprKind::Reference(_) => {},
        ExprKind::BinaryOp(_, _, _) => {},
        ExprKind::FunctionCall(ident, args) => {
            for a in args {
                let ar = &(*a.ptr);
                if matches!(ar.kind, ExprKind::FunctionCall(_, _)) {
                    let println_arg_result = check_expr_types(ar, types, functions);
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
                    match ar.kind {
                        ExprKind::Function(_, _, _, _) => {},
                        ExprKind::If(_, _, _) => {},
                        ExprKind::IntConstant(_) => {
                            if !check_type("int".to_string(), &f.parameters[i]) {
                                result.success.set(false);
                                result.errors.borrow_mut().push(format!("Error: Wrong argument type '{}' for parameter '{}: {}' in call to function '{}'", "int", f.parameters[i].name, f.parameters[i].typ.name, f.name));
                                return result;
                            }
                        },
                        ExprKind::StringConstant(_) => {
                            if !check_type("string".to_string(), &f.parameters[i]) {
                                result.success.set(false);
                                result.errors.borrow_mut().push(format!("Error: Wrong argument type '{}' for parameter '{}: {}' in call to function '{}'", "string", f.parameters[i].name, f.parameters[i].typ.name, f.name));
                                return result;
                            }
                        },
                        ExprKind::Return(_) => {},
                        ExprKind::Reference(_) => {},
                        ExprKind::BinaryOp(_, _, _) => {},
                        ExprKind::FunctionCall(_, _) => {},
                    }
                    i += 1;
                }
                // TODO: actually type check arguments
            }
        }
    };
    /*
    result.success.set(false);
    result.errors.borrow_mut().push("big expr error!".into());
    */
    return result;
}

fn check_type(argument: String, parameter: &TCParameter) -> bool {
    if Rc::clone(&parameter.typ).name == "any".to_string() {
        return true;
    }
    if Rc::clone(&parameter.typ).name == argument {
        return true;
    }
    return false;
}

fn type_exists(typ: String, types: &HashMap<String, Rc<TCType>>) -> bool {
    return true;
}

fn build_types(ast: &Root) -> HashMap<String, Rc<TCType>> {
    let mut types = HashMap::new();
    types.insert("any".into(), Rc::new(TCType {
        name: "any".into(),
        base_type: None,
    }));
    types.insert("string".into(), Rc::new(TCType {
        name: "string".into(),
        base_type: None,
    }));
    types.insert("int".into(), Rc::new(TCType {
        name: "int".into(),
        base_type: None,
    }));
    types.insert("double".into(), Rc::new(TCType {
        name: "double".into(),
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
                        },
                        None => {
                            result.success.set(false);
                            result.errors.borrow_mut().push(format!("Error: type '{}' does not exist -- found as parameter '{}' of function '{}'", cl_pa.1, cl_pa.0, ident));
                            return (result, functions);
                        },
                    }
                }
                let tc_return_type = types.get(return_type.as_str());
                let ret_type = match tc_return_type  {
                    Some(t) => {
                            Rc::clone(t)
                    },
                    None => {
                        result.success.set(false);
                        result.errors.borrow_mut().push(format!("Error: type '{}' does not exist -- found as return type of function '{}'", return_type, ident));
                        return (result, functions);
                    },
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
