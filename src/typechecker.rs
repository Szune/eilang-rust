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
use crate::ast::{BinaryOp, Block, Comparison, Expr, ExprKind, FunctionExpr, Ptr, Root};
use crate::types::{Type, TypeCollector};
use std::collections::HashSet;
use std::ops::Deref;

fn determine_types<'a>(
    root: &'a mut Root,
    types: &'a mut TypeCollector,
) -> (&'a mut Root, &'a mut TypeCollector) {
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
                        let new_type =
                            types.get_type(&p.ptr.deref().typ.name, &p.ptr.deref().typ.scope);
                        assert_determined!(new_type);
                        p.ptr.typ = new_type;
                    }
                }
            }
            _ => unreachable!(),
        }
    }
    // determine all indeterminate types
    (root, types)
}

/// Type checks most of the expressions in the code
///
/// Takes builtins so that eilang can be used as a library in the future, where you can write your own builtins
///
/// ```
/// # let code = "fn test(s: int) -> int { return s; }";
/// # let lexer = crate::lexer::Lexer::new(code.into());
/// # let (mut ast, mut types) = crate::parser::Parser::parse(lexer);
/// let checked = check_types(&mut ast, &mut types, &crate::builtins::get_names());
/// assert!(checked.is_ok());
/// ```
pub fn check_types(
    root: &mut Root,
    types: &mut TypeCollector,
    builtins: &HashSet<String>,
) -> Result<(), String> {
    let (root, types) = determine_types(root, types);

    // TODO: if there's an indeterminate type at this point, panic!
    for f in &root.functions {
        match &f.kind {
            ExprKind::Function(fun) => {
                check_block(fun, &fun.code, root, types, builtins)?;
            }
            _ => unreachable!(),
        }
    }
    Ok(())
}

fn check_block(
    func: &FunctionExpr,
    block: &Ptr<Block>,
    root: &Root,
    types: &TypeCollector,
    builtins: &HashSet<String>,
) -> Result<(), String> {
    for (i, expr) in block.ptr.exprs.iter().enumerate() {
        check_expr(func, expr, i, root, types, &builtins)?
    }
    Ok(())
}

fn check_expr(
    func: &FunctionExpr,
    expr: &Expr,
    expr_index: usize,
    root: &Root,
    types: &TypeCollector,
    builtins: &HashSet<String>,
) -> Result<(), String> {
    match &expr.kind {
        ExprKind::Return(r) => {
            if func.return_type.id == types.any().id {
                return Ok(());
            }
            if let Some(r) = r {
                let returned_type = find_expr(func, &r, expr_index, root, types)?;
                if returned_type.id != func.return_type.id {
                    return Err(format!("Tried to return item of type {:?} in function {} expecting return type {:?}", returned_type, func.name, func.return_type));
                }
            } else if func.return_type.id != types.unit().id {
                return Err(format!(
                    "Tried to return unit in function {} expecting return type {:?}",
                    func.name, func.return_type
                ));
            }
            Ok(())
        }
        ExprKind::Function(_) => Ok(()), // functions in functions are not implemented yet
        ExprKind::NewAssignment(_, v) => {
            check_expr(func, v.ptr.deref(), expr_index, root, types, builtins)?;
            Ok(())
        }
        ExprKind::Reassignment(ident, v) => {
            check_expr(func, v.ptr.deref(), expr_index, root, types, builtins)?;
            let re_typ = find_expr(func, v, expr_index, root, types)?;
            let typ = find_reference(func, ident, expr_index, root, types)?;
            if typ.id != re_typ.id {
                return Err(format!(
                    "Cannot use {:?} for variable {} already defined as {:?}",
                    re_typ.name, ident, typ.name
                ));
            }
            Ok(())
        }
        ExprKind::If(if_expr, block) => {
            check_expr(func, if_expr.ptr.deref(), expr_index, root, types, builtins)?;
            let if_type = find_expr(func, if_expr, expr_index, root, types)?;
            if if_type.id != types.boolean().id {
                return Err(format!(
                    "Cannot use {:?} in an if statement because it is not a bool value",
                    if_type.name
                ));
            }
            check_block(func, block, root, types, builtins)?;
            Ok(())
        }
        ExprKind::IfElse(if_expr, true_block, else_block) => {
            check_expr(func, if_expr.ptr.deref(), expr_index, root, types, builtins)?;
            let if_type = find_expr(func, if_expr, expr_index, root, types)?;
            if if_type.id != types.boolean().id {
                return Err(format!(
                    "Cannot use {:?} in an if statement because it is not a bool value",
                    if_type.name
                ));
            }
            check_block(func, true_block, root, types, builtins)?;
            check_block(func, else_block, root, types, builtins)?;
            Ok(())
        }
        ExprKind::IfElseIf(if_expr, block, else_if_exprs) => {
            check_expr(func, if_expr.ptr.deref(), expr_index, root, types, builtins)?;
            let if_type = find_expr(func, if_expr, expr_index, root, types)?;
            if if_type.id != types.boolean().id {
                return Err(format!(
                    "Cannot use {:?} in an if statement because it is not a bool value",
                    if_type.name
                ));
            }
            check_block(func, block, root, types, builtins)?;
            for else_if in else_if_exprs.iter() {
                let if_type = find_expr(func, &else_if.ptr.if_expr, expr_index, root, types)?;
                if if_type.id != types.boolean().id {
                    return Err(format!(
                        "Cannot use {:?} in an if statement because it is not a bool value",
                        if_type.name
                    ));
                }
                check_block(func, &else_if.ptr.block, root, types, builtins)?;
            }
            Ok(())
        }
        ExprKind::IfElseIfElse(if_expr, block, else_if_exprs, else_block) => {
            check_expr(func, if_expr.ptr.deref(), expr_index, root, types, builtins)?;
            let if_type = find_expr(func, if_expr, expr_index, root, types)?;
            if if_type.id != types.boolean().id {
                return Err(format!(
                    "Cannot use {:?} in an if statement because it is not a bool value",
                    if_type.name
                ));
            }
            check_block(func, block, root, types, builtins)?;
            for else_if in else_if_exprs.iter() {
                let if_type = find_expr(func, &else_if.ptr.if_expr, expr_index, root, types)?;
                if if_type.id != types.boolean().id {
                    return Err(format!(
                        "Cannot use {:?} in an if statement because it is not a bool value",
                        if_type.name
                    ));
                }
                check_block(func, &else_if.ptr.block, root, types, builtins)?;
            }
            check_block(func, else_block, root, types, builtins)?;
            Ok(())
        }
        ExprKind::BoolConstant(_) => Ok(()), // bool constant on its own can't have the wrong type
        ExprKind::IntConstant(_) => Ok(()),  // int constant on its own can't have the wrong type
        ExprKind::StringConstant(_) => Ok(()), // string constant on its own can't have the wrong type
        ExprKind::UnitConstant => Ok(()),      // unit constant on its own can't have the wrong type
        ExprKind::StackPop => Ok(()),
        ExprKind::Reference(_) => Ok(()),
        ExprKind::Comparison(l, r, op) => {
            check_expr(func, l.ptr.deref(), expr_index, root, types, builtins)?;
            check_expr(func, r.ptr.deref(), expr_index, root, types, builtins)?;
            let l = find_expr(func, &l, expr_index, root, types)?;
            let r = find_expr(func, &r, expr_index, root, types)?;
            if !are_comparable(&l, &r, op, types) {
                return Err(format!(
                    "Cannot {:?} compare a {:?} value with a {:?} value",
                    op, l.name, r.name
                ));
            }
            Ok(())
        }
        ExprKind::BinaryOp(l, r, op) => {
            check_expr(func, l.ptr.deref(), expr_index, root, types, builtins)?;
            check_expr(func, r.ptr.deref(), expr_index, root, types, builtins)?;
            let l = find_expr(func, &l, expr_index, root, types)?;
            let r = find_expr(func, &r, expr_index, root, types)?;
            let _ = binary_op_result_type_of(l, r, op, types)?;
            Ok(())
        }
        ExprKind::FunctionCall(n, args) => {
            if builtins.contains(n) {
                // can't access the code, just check the expressions in the arguments
                for arg in args.iter() {
                    check_expr(func, arg.ptr.deref(), expr_index, root, types, builtins)?;
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
        (l, r)
            if l == types.string().id
                && r == types.string().id
                && *op != Comparison::Equals
                && *op != Comparison::NotEquals =>
        {
            false
        }
        (l, r) if l == types.string().id && r == types.string().id => true,
        (l, r) if l == types.int64().id && r == types.int64().id => true,
        (l, r) if l == r => true,
        (l, r) if l != r => false,
        (_, _) => false,
    }
}

fn find_expr(
    func: &FunctionExpr,
    expr: &Ptr<Expr>,
    expr_index: usize,
    root: &Root,
    types: &TypeCollector,
) -> Result<Type, String> {
    Ok(match &expr.ptr.kind {
        ExprKind::Function(_) => types.unit(), // at a later point function declarations should be expressions
        ExprKind::If(..) => types.unit(), // for now, I do like the idea of ifs as expressions though
        ExprKind::IfElse(..) => types.unit(),
        ExprKind::IfElseIf(..) => types.unit(),
        ExprKind::IfElseIfElse(..) => types.unit(),
        ExprKind::BoolConstant(_) => types.boolean(),
        ExprKind::IntConstant(_) => types.int64(),
        ExprKind::StringConstant(_) => types.string(),
        ExprKind::UnitConstant => types.unit(),
        ExprKind::StackPop => types.any(),
        ExprKind::Comparison(..) => types.boolean(),
        ExprKind::NewAssignment(_, v) => find_expr(func, &v, expr_index, root, types)?,
        ExprKind::Reassignment(_, v) => find_expr(func, &v, expr_index, root, types)?,
        ExprKind::Return(v) => {
            if let Some(v) = v {
                find_expr(func, &v, expr_index, root, types)?
            } else {
                types.unit()
            }
        }
        ExprKind::Reference(ident) => find_reference(func, &ident, expr_index, root, types)?,
        ExprKind::BinaryOp(left, right, op) => {
            let left = find_expr(func, &left, expr_index, root, types)?;
            let right = find_expr(func, &right, expr_index, root, types)?;
            binary_op_result_type_of(left, right, op, types)?
        }
        ExprKind::FunctionCall(func_to_call, ..) => find_function(func_to_call.as_str(), root)?
            .return_type
            .clone(),
    })
}

fn find_function<'a>(name: &str, root: &'a Root) -> Result<&'a FunctionExpr, String> {
    if let Some(fun) = root
        .functions
        .iter()
        .map(|f| match &f.kind {
            ExprKind::Function(f) => f,
            _ => unreachable!(),
        })
        .find(|p| p.name == name)
    {
        Ok(fun)
    } else {
        Err(format!("Could not find function {}", name))
    }
}

fn find_reference(
    func: &FunctionExpr,
    ident: &str,
    ref_index: usize,
    root: &Root,
    types: &TypeCollector,
) -> Result<Type, String> {
    // 1. check local scope
    // 2. check arguments
    // 3. check global scope

    for e in func.code.ptr.exprs.iter().take(ref_index).rev() {
        // rev to get the closest one in case of shadowing
        if let ExprKind::NewAssignment(n, v) = &e.kind {
            if n == ident {
                return find_expr(func, v, ref_index, root, types);
            }
        }
    }

    for p in &func.parameters {
        if p.ptr.name == ident {
            return Ok(p.ptr.typ.clone());
        }
    }
    Err(format!("Could not find reference {}", ident))
}

fn binary_op_result_type_of(
    left: Type,
    right: Type,
    op: &BinaryOp,
    types: &TypeCollector,
) -> Result<Type, String> {
    // TODO: rewrite this to be more performant, the current iteration is pretty awful
    match (left.id, right.id) {
        (l, _) if l == types.string().id => Ok(types.string()),
        (l, r) if l == types.int64().id && r == types.int64().id => Ok(types.int64()),
        (l, r) if l == r => Ok(left),
        (l, r) if l != r => Err(format!(
            "incompatible types in binary op {:?} ({} and {})",
            op, left.name, right.name
        )),
        (_, _) => Ok(types.unit()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::builtins;
    use crate::types::TypeScope;

    #[test]
    pub fn returning_correct_type_in_function_is_ok() {
        let code = r#"
        fn test(s: cool_type) -> cool_type {
            return s;
        }

        println("hello!");"#;
        let lexer = crate::lexer::Lexer::new(code.into());
        let (mut ast, mut types) = crate::parser::Parser::parse(lexer);
        types
            .try_define_type("cool_type", TypeScope::Global)
            .unwrap();
        assert!(check_types(&mut ast, &mut types, &builtins::get_names()).is_ok());
    }

    #[test]
    pub fn returning_wrong_type_in_function_is_err() {
        let code = r#"
        fn test(s: _type) -> cool_type {
            return s;
        }

        println("hello!");"#;
        let lexer = crate::lexer::Lexer::new(code.into());
        let (mut ast, mut types) = crate::parser::Parser::parse(lexer);
        types.try_define_type("_type", TypeScope::Global).unwrap();
        types
            .try_define_type("cool_type", TypeScope::Global)
            .unwrap();
        assert!(check_types(&mut ast, &mut types, &builtins::get_names()).is_err());
    }

    #[test]
    pub fn returning_coerced_string_is_ok() {
        let code = r#"
        fn test(a: int, b: string) -> string {
            return b + a;
        }

        hello := test(2,"hi");"#;
        let lexer = crate::lexer::Lexer::new(code.into());
        let (mut ast, mut types) = crate::parser::Parser::parse(lexer);
        assert!(check_types(&mut ast, &mut types, &builtins::get_names()).is_ok());
    }

    #[test]
    pub fn returning_int_when_expecting_string_is_err() {
        let code = r#"
        fn test(a: int, b: int) -> string {
            return b + a;
        }

        hello := test(2,3);"#;
        let lexer = crate::lexer::Lexer::new(code.into());
        let (mut ast, mut types) = crate::parser::Parser::parse(lexer);
        assert!(check_types(&mut ast, &mut types, &builtins::get_names()).is_err());
    }

    #[test]
    pub fn if_expr_with_bool_result_is_ok() {
        let code = r#"
        if 1 == 1 { }
        "#;
        let lexer = crate::lexer::Lexer::new(code.into());
        let (mut ast, mut types) = crate::parser::Parser::parse(lexer);
        assert!(check_types(&mut ast, &mut types, &builtins::get_names()).is_ok());
    }

    #[test]
    pub fn if_string_expr_is_err() {
        let code = r#"
        if "true" { }
        "#;
        let lexer = crate::lexer::Lexer::new(code.into());
        let (mut ast, mut types) = crate::parser::Parser::parse(lexer);
        assert!(check_types(&mut ast, &mut types, &builtins::get_names()).is_err());
    }

    #[test]
    pub fn if_bool_expr_is_ok() {
        let code = r#"
        if true { }
        if false { }
        "#;
        let lexer = crate::lexer::Lexer::new(code.into());
        let (mut ast, mut types) = crate::parser::Parser::parse(lexer);
        assert!(check_types(&mut ast, &mut types, &builtins::get_names()).is_ok());
    }

    #[test]
    pub fn local_reference_is_checked_is_ok() {
        let code = r#"
        val := 1;
        if val == 500 { }
        "#;
        let lexer = crate::lexer::Lexer::new(code.into());
        let (mut ast, mut types) = crate::parser::Parser::parse(lexer);
        assert!(check_types(&mut ast, &mut types, &builtins::get_names()).is_ok());
    }

    #[test]
    pub fn local_reference_is_checked_is_err() {
        let code = r#"
        val := 1;
        if val == "blue" { }
        "#;
        let lexer = crate::lexer::Lexer::new(code.into());
        let (mut ast, mut types) = crate::parser::Parser::parse(lexer);
        assert!(check_types(&mut ast, &mut types, &builtins::get_names()).is_err());
    }

    #[test]
    pub fn local_reference_in_function_is_checked_is_ok() {
        let code = r#"
        fn test() {
            val := 1;
            if val == 500 { }
        }
        test();
        "#;
        let lexer = crate::lexer::Lexer::new(code.into());
        let (mut ast, mut types) = crate::parser::Parser::parse(lexer);
        assert!(check_types(&mut ast, &mut types, &builtins::get_names()).is_ok());
    }

    #[test]
    pub fn local_reference_in_function_is_checked_is_err() {
        let code = r#"
        fn test() {
            val := "blue";
            if val == false { }
        }
        test();
        "#;
        let lexer = crate::lexer::Lexer::new(code.into());
        let (mut ast, mut types) = crate::parser::Parser::parse(lexer);
        assert!(check_types(&mut ast, &mut types, &builtins::get_names()).is_err());
    }

    #[test]
    pub fn shadowed_variable_is_ok() {
        let code = r#"
        fn test(val: int) {
            val := "blue";
            if val == "red" { }
        }
        test(1);
        "#;
        let lexer = crate::lexer::Lexer::new(code.into());
        let (mut ast, mut types) = crate::parser::Parser::parse(lexer);
        assert!(check_types(&mut ast, &mut types, &builtins::get_names()).is_ok());
    }

    #[test]
    pub fn shadowed_variable_is_err() {
        let code = r#"
        val := "blue";
        val := 1;
        if val == "red" { }
        "#;
        let lexer = crate::lexer::Lexer::new(code.into());
        let (mut ast, mut types) = crate::parser::Parser::parse(lexer);
        assert!(check_types(&mut ast, &mut types, &builtins::get_names()).is_err());
    }

    #[test]
    pub fn reassignment_is_ok() {
        let code = r#"
        val := "blue";
        val = "red";
        if val == "red" { }
        "#;
        let lexer = crate::lexer::Lexer::new(code.into());
        let (mut ast, mut types) = crate::parser::Parser::parse(lexer);
        assert!(check_types(&mut ast, &mut types, &builtins::get_names()).is_ok());
    }

    #[test]
    pub fn reassignment_is_err() {
        let code = r#"
        val := "blue";
        val = 1;
        if val == 2 { }
        "#;
        let lexer = crate::lexer::Lexer::new(code.into());
        let (mut ast, mut types) = crate::parser::Parser::parse(lexer);
        assert!(check_types(&mut ast, &mut types, &builtins::get_names()).is_err());
    }
}
