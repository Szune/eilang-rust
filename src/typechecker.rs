use crate::ast::{Root, ExprKind, Expr, FunctionExpr, Ptr, BinaryOp, Comparison, Block};
use crate::types::{TypeCollector,Type};
use std::ops::Deref;

fn determine_types<'a>(root: &'a mut Root, types: &'a mut TypeCollector) -> (&'a mut Root, &'a mut TypeCollector) {
    macro_rules! assert_determined (
        ($t:path) => {
            if $t.is_indeterminate() {
                panic!("Used undefined type {}", $t.name);
            }
        }
    );
    // at this point, the parser should have defined all available types, so if a type cannot be found here,
    // there's either an error in the parser, the eilang code, or there's a new feature or bug causing it
    for f in root.functions.iter_mut() {
        match &mut f.kind {
            ExprKind::Function(ref mut fun) => {
                if fun.return_type.is_indeterminate() {
                    let return_type = types.get_type(&fun.return_type.name, &fun.return_type.scope);
                    assert_determined!(return_type);
                    fun.return_type = return_type;
                }
                for p in fun.parameters.iter_mut() {
                    if p.ptr.deref().typ.is_indeterminate() {
                        let new_type = types.get_type(&p.ptr.deref().typ.name, &p.ptr.deref().typ.scope);
                        assert_determined!(new_type);
                        p.ptr.typ = new_type;
                    }
                }
            }
            _ => unreachable!()
        }
    }
    // determine all indeterminate types
    (root, types)
}

pub fn check_types(root: &mut Root, types: &mut TypeCollector) -> Result<(), String> {
    let (root, types) = determine_types(root, types);

    // TODO: if there's an indeterminate type at this point, panic!
    for f in &root.functions {
        match &f.kind {
            ExprKind::Function(fun) => {
                check_block(fun, &fun.code, root, types)?;
            }
            _ => unreachable!(),
        }
    }
    Ok(())
}

fn check_block(func: &FunctionExpr, block: &Ptr<Block>, root: &Root, types: &TypeCollector) -> Result<(), String> {
    for (i, expr) in block.ptr.exprs.iter().enumerate() {
        check_expr(func, expr, i, root, types)?
    }
    Ok(())
}

fn check_expr(func: &FunctionExpr, expr: &Expr, expr_index: usize, root: &Root, types: &TypeCollector) -> Result<(), String> {
    match &expr.kind {
        ExprKind::Return(r) =>
            {
                if let Some(r) = r {
                    let returned_type = find_expr(func, &r, expr_index, root, types)?;
                    if returned_type.id != func.return_type.id {
                        return Err(format!("Tried to return item of type {:?} in function {} expecting return type {:?}", returned_type, func.name, func.return_type));
                    }
                }
                Ok(())
            }
        ExprKind::Function(_) => Ok(()), // functions in functions are not implemented yet
        ExprKind::NewAssignment(_, v) => {
            check_expr(func, v.ptr.deref(), expr_index, root, types)?;
            Ok(())
        }
        ExprKind::If(_if, v, e) => {
            check_expr(func, _if.ptr.deref(), expr_index, root, types)?;
            let if_type = find_expr(func, _if, expr_index, root, types)?;
            if if_type.id != types.boolean().id {
                return Err(format!("Cannot use {:?} in an if statement because it is not a bool value", if_type.name));
            }

            check_block(func, v, root, types)?;
            if let Some(e) = e {
                check_block(func, e, root, types)?;
            }
            Ok(())
        }
        ExprKind::IntConstant(_) => Ok(()), // int constant on its own can't have the wrong type
        ExprKind::StringConstant(_) => Ok(()), // string constant on its own can't have the wrong type
        ExprKind::Reference(_) => Ok(()),
        ExprKind::Comparison(l, r, op) => {
            check_expr(func, l.ptr.deref(), expr_index, root, types)?;
            check_expr(func, r.ptr.deref(), expr_index, root, types)?;
            let l = find_expr(func, &l, expr_index, root, types)?;
            let r = find_expr(func, &r, expr_index, root, types)?;
            if !are_comparable(&l, &r, op, types) {
                return Err(format!("Cannot {:?} compare a {:?} value with a {:?} value", op, l.name, r.name));
            }
            Ok(())
        }
        ExprKind::BinaryOp(l, r, op) => {
            check_expr(func, l.ptr.deref(), expr_index, root, types)?;
            check_expr(func, r.ptr.deref(), expr_index, root, types)?;
            let l = find_expr(func, &l, expr_index, root, types)?;
            let r = find_expr(func, &r, expr_index, root, types)?;
            let _ = binary_op_result_type_of(l, r, op, types)?;
            Ok(())
        }
        ExprKind::FunctionCall(n, args) => {
            if n == "println" {
                // special case, just check any expressions being printed :(
                for arg in args.iter() {
                    check_expr(func, arg.ptr.deref(), expr_index, root, types)?;
                }
                Ok(())
            } else {
                let func = find_function(n, root)?;
                for (param, arg) in func.parameters.iter().zip(args) {
                    let arg = find_expr(func, arg, expr_index, root, types)?;
                    if param.ptr.typ.id != arg.id {
                        return Err(format!("Cannot call function {} with type {:?} for parameter {} because it expects type {:?}",
                                           func.name, arg.name, param.ptr.name, param.ptr.typ.name));
                    }
                }
                Ok(())
            }
        }
    }
}

fn are_comparable(left: &Type, right: &Type, op: &Comparison, types: &TypeCollector) -> bool {
    match (left.id, right.id) {
        (l, r) if l == types.string().id && r == types.string().id
            && *op != Comparison::Equals && *op != Comparison::NotEquals => false,
        (l, r) if l == types.string().id && r == types.string().id => true,
        (l, r) if l == types.int64().id && r == types.int64().id => true,
        (l, r) if l == r => true,
        (l, r) if l != r => false,
        (_, _) => false,
    }
}

fn find_expr(func: &FunctionExpr, expr: &Ptr<Expr>, expr_index: usize, root: &Root, types: &TypeCollector) -> Result<Type, String> {
    Ok(match &expr.ptr.kind {
        ExprKind::Function(_) => types.unit(), // at a later point function declarations should be expressions
        ExprKind::If(_, _, _) => types.unit(), // for now, I do like the idea of ifs as expressions though
        ExprKind::IntConstant(_) => types.int64(),
        ExprKind::StringConstant(_) => types.string(),
        ExprKind::Comparison(_, _, _) => types.boolean(),
        ExprKind::NewAssignment(_, v) => find_expr(func, &v, expr_index, root, types)?,
        ExprKind::Return(v) => {
            if let Some(v) = v {
                find_expr(func, &v, expr_index, root, types)?
            } else {
                types.unit()
            }
        },
        ExprKind::Reference(ident) => {
            find_reference(func, &ident, expr_index, root, types)?
        }
        ExprKind::BinaryOp(left, right, op) => {
            let left = find_expr(func, &left, expr_index, root, types)?;
            let right = find_expr(func, &right, expr_index, root, types)?;
            binary_op_result_type_of(left, right, op, types)?
        }
        ExprKind::FunctionCall(func_to_call, _) => {
            find_function(func_to_call.as_str(), root)?
                .return_type
                .clone()
        }
    })
}

fn find_function<'a>(name: &str, root: &'a Root) -> Result<&'a FunctionExpr, String> {
    if let Some(fun) = root.functions.iter()
        .map(|f| match &f.kind {
            ExprKind::Function(f) => f,
            _ => unreachable!()
        })
        .find(|p| p.name == name) {
        Ok(fun)
    } else {
        Err(format!("Could not find function {}", name))
    }
}

fn find_reference(func: &FunctionExpr, ident: &str, ref_index: usize, root: &Root, types: &TypeCollector) -> Result<Type, String> {
    // 1. check local scope
    // 2. check arguments
    // 3. check global scope

    for e in func.code.ptr.exprs.iter().take(ref_index) {
        match &e.kind {
            ExprKind::NewAssignment(n, v) => {
                if n == ident {
                    return find_expr(func, v, ref_index, root, types);
                }
            }
            _ => ()
        }
    }


    for p in &func.parameters {
        if p.ptr.name == ident {
            return Ok(p.ptr.typ.clone());
        }
    }
    Err("Could not find reference".into())
}

fn binary_op_result_type_of(left: Type, right: Type, op: &BinaryOp, types: &TypeCollector) -> Result<Type,String> {
    // TODO: rewrite this to be more performant, the current iteration is pretty awful
    match (left.id, right.id) {
        (l, _) if l == types.string().id => Ok(types.string()),
        (l, r) if l == types.int64().id && r == types.int64().id => Ok(types.int64()),
        (l, r) if l == r => Ok(left),
        (l, r) if l != r => Err(format!("incompatible types in binary op {:?} ({} and {})", op, left.name, right.name)),
        (_, _) => Ok(types.unit())
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::TypeScope;

    #[test]
    pub fn test() {
        let code = r#"
        fn test(s: cool_type) -> cool_type {
            return s;
        }

        println("hello!");"#;
        let lexer = crate::lexer::Lexer::new(code.into());
        let (mut ast, mut types) = crate::parser::Parser::parse(lexer);
        types.try_define_type("_type", TypeScope::Global).unwrap();
        types.try_define_type("cool_type", TypeScope::Global).unwrap();
        assert!(check_types(&mut ast, &mut types));
    }

    #[test]
    #[should_panic(expected = "Tried to return item of type Type { id: 10, name: \"_type\", scope: Global } in function test expecting return type Type { id: 11, name: \"cool_type\", scope: Global }")]
    pub fn test_negative() {
        let code = r#"
        fn test(s: _type) -> cool_type {
            return s;
        }

        println("hello!");"#;
        let lexer = crate::lexer::Lexer::new(code.into());
        let (mut ast, mut types) = crate::parser::Parser::parse(lexer);
        types.try_define_type("_type", TypeScope::Global).unwrap();
        types.try_define_type("cool_type", TypeScope::Global).unwrap();
        check_types(&mut ast, &mut types);
    }
}
