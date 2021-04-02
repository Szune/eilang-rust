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

use crate::ast::{Block, Expr, ExprKind, Ptr, Root};

/// Tries to optimize the AST before compiling it
pub fn optimize(ast: &mut Root) {
    for f in &mut ast.functions {
        match &mut f.kind {
            ExprKind::Function(ref mut fun) => {
                // if I'm not missing something it should be okay to always add a pop after a function call if the function call is a "root" expression
                optimize_root_expr(&mut fun.code);
            }
            _ => unreachable!(),
        }
    }
}

fn optimize_root_expr(block: &mut Ptr<Block>) {
    let mut offsets = Vec::new();
    for (i, expr) in &mut block.ptr.exprs.iter_mut().enumerate() {
        match &mut expr.kind {
            ExprKind::FunctionCall(_, _) => offsets.push(i),
            ExprKind::If(_, ref mut a) => {
                optimize_root_expr(a);
            }
            ExprKind::IfElse(_, ref mut a, ref mut b) => {
                optimize_root_expr(a);
                optimize_root_expr(b);
            }
            ExprKind::IfElseIf(_, ref mut a, ref mut b) => {
                optimize_root_expr(a);
                for e in b.iter_mut() {
                    optimize_root_expr(&mut e.ptr.block);
                }
            }
            ExprKind::IfElseIfElse(_, ref mut a, ref mut b, ref mut c) => {
                optimize_root_expr(a);
                for e in b.iter_mut() {
                    optimize_root_expr(&mut e.ptr.block);
                }
                optimize_root_expr(c);
            }
            _ => (),
        }
    }
    for (offset, insert_idx) in offsets.into_iter().enumerate() {
        block
            .ptr
            .exprs
            .insert(insert_idx + offset + 1, Expr::new(ExprKind::StackPop)); // clean up stack (might not always be beneficial tbh)
    }
}
