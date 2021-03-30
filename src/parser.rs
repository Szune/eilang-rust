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
use crate::lexer::Lexer;
use crate::token::*;
use crate::types::{TypeCollector, TypeScope};
use std::ops::Deref;
use crate::function::Parameter;

pub struct Parser {
    lexer: Lexer,
    buffer: [Token; 2],
    pub root: Root,
    types: TypeCollector,
}

impl Parser {
    pub fn parse(lexer: Lexer) -> (Root,TypeCollector) {
        let mut parser = Parser {
            lexer,
            buffer: [Token::empty(), Token::empty()],
            root: Root::new(),
            types: TypeCollector::new(),
        };
        parser.consume();
        parser.consume();
        parser.parse_private();
        return (parser.root,parser.types);
    }

    fn parse_private(&mut self) {
        self.parse_global_scope();
    }

    fn parse_global_scope(&mut self) {
        let mut global_block = Block::new();
        while !self.is_token(TokenType::EndOfText) {
            match &self.buffer[0].typ {
                TokenType::Function => {
                    let func = self.parse_global_function();
                    //println!("Added global {:?}", func.kind);
                    self.root.functions.push(func);
                }
                TokenType::Semicolon => self.consume(),
                _ => {
                    let expr = self.parse_root_expr();
                    global_block.exprs.push(expr);
                }
            };
        }
        global_block.exprs.push(Expr::new(ExprKind::Return(None)));
        let any = self.types.any();
        self.root.functions.push(
            Expr::new(ExprKind::Function(FunctionExpr {
                name: ".main".into(),
                return_type: any.clone(),
                parameters: Vec::new(),
                code: Ptr(global_block)
            })));
    }

    fn parse_global_function(&mut self) -> Expr {
        self.require(TokenType::Function);
        let ident = self.parse_full_identifier();
        //println!("function identifier: {}", ident.string.clone().unwrap());
        let parameters = self.parse_parameter_list();
        let parameters = parameters.iter()
            .map(|p| {
                let (arg, typ) = p.ptr.deref().clone();
                let typ =
                    self.types
                        .get_type(&typ, &TypeScope::Global);
                Ptr(Parameter {
                    name: arg,
                    typ,
                })
            }).collect();
        let return_type = if self.is_token(TokenType::Arrow) {
            self.require(TokenType::Arrow);
            let return_type = self.parse_full_identifier();
            self.types.get_type(&return_type, &TypeScope::Global)
        } else {
            // if return type is not specified, implicitly set it to Unit
            self.types.unit()
        };
        /*println!(
            "function ident: '{}', function return type: '{}'",
            ident.string.clone().unwrap(),
            return_type.string.clone().unwrap()
        );*/
        let block = self.parse_block();
        Expr::new(ExprKind::Function(FunctionExpr {
            name: ident,
            return_type,
            parameters,
            code: block,
        }))
    }

    fn parse_block(&mut self) -> Ptr<Block> {
        let mut block = Block::new();
        self.require(TokenType::LeftBrace);
        while !self.is_token(TokenType::RightBrace) && !self.is_token(TokenType::EndOfText) {
            let expr = self.parse_root_expr();
            block.exprs.push(expr);
        }
        self.require(TokenType::RightBrace);
        return Ptr(block);
    }

    /// e.g. if, return, etc
    fn parse_root_expr(&mut self) -> Expr {
        match self.buffer[0].typ {
            TokenType::Return => {
                self.consume();
                if self.is_token(TokenType::Semicolon) {
                    self.consume();
                    return Expr::new(ExprKind::Return(None));
                }
                let expr = self.parse_or();
                self.require(TokenType::Semicolon);
                return Expr::new(ExprKind::Return(Some(Ptr(expr))));
            }
            TokenType::If => {
                self.consume();
                let if_expr = self.parse_or();
                let true_block = self.parse_block();
                let else_block =
                    match self.buffer[0].typ {
                        TokenType::Else => {
                            self.consume();
                            Some(self.parse_block())
                        }
                        _ => None,
                    };

                return Expr::new(ExprKind::If(Ptr(if_expr), true_block, else_block));
            }
            TokenType::Identifier(_) => {
                let expr = self.parse_outermost_expr();
                self.require(TokenType::Semicolon);
                expr
            }
            _ => {
                let expr = self.parse_outermost_expr();
                expr
            }
        }
    }

    fn parse_or(&mut self) -> Expr {
        let mut expr = self.parse_and();
        while matches!(self.buffer[0].typ, TokenType::Or) {
            self.consume();
            let right = self.parse_and();
            expr = Expr::new(ExprKind::Comparison(Ptr(expr), Ptr(right), Comparison::Or))
        }
        expr
    }

    fn parse_and(&mut self) -> Expr {
        let mut expr = self.parse_equality_comparisons();
        while matches!(self.buffer[0].typ, TokenType::And) {
            self.consume();
            let right = self.parse_equality_comparisons();
            expr = Expr::new(ExprKind::Comparison(Ptr(expr), Ptr(right), Comparison::And))
        }
        expr
    }

    fn parse_equality_comparisons(&mut self) -> Expr {
        let mut expr = self.parse_difference_comparisons();
        while matches!(self.buffer[0].typ, TokenType::EqualsEquals | TokenType::NotEquals) {
            if self.is_token(TokenType::EqualsEquals) {
                self.consume();
                let right = self.parse_difference_comparisons();
                expr = Expr::new(ExprKind::Comparison(Ptr(expr), Ptr(right), Comparison::Equals))
            } else if self.is_token(TokenType::NotEquals) {
                self.consume();
                let right = self.parse_difference_comparisons();
                expr = Expr::new(ExprKind::Comparison(Ptr(expr), Ptr(right), Comparison::NotEquals))
            }
        }
        expr
    }

    fn parse_difference_comparisons(&mut self) -> Expr {
        let mut expr = self.parse_binary_ops_1();

        while matches!(self.buffer[0].typ,
         TokenType::LessThan | TokenType::GreaterThan | TokenType::LessThanEquals | TokenType::GreaterThanEquals) {
            if self.is_token(TokenType::LessThan) {
                self.consume();
                let right = self.parse_binary_ops_1();
                expr = Expr::new(ExprKind::Comparison(Ptr(expr), Ptr(right), Comparison::LessThan))
            } else if self.is_token(TokenType::GreaterThan) {
                self.consume();
                let right = self.parse_binary_ops_1();
                expr = Expr::new(ExprKind::Comparison(Ptr(expr), Ptr(right), Comparison::GreaterThan))
            } else if self.is_token(TokenType::LessThanEquals) {
                self.consume();
                let right = self.parse_binary_ops_1();
                expr = Expr::new(ExprKind::Comparison(Ptr(expr), Ptr(right), Comparison::LessThanEquals))
            } else if self.is_token(TokenType::GreaterThanEquals) {
                self.consume();
                let right = self.parse_binary_ops_1();
                expr = Expr::new(ExprKind::Comparison(Ptr(expr), Ptr(right), Comparison::GreaterThanEquals))
            }
        }
        expr
    }

    /// e.g. binary ops, etc
    fn parse_outermost_expr(&mut self) -> Expr {
        match (&self.buffer[0].typ, &self.buffer[1].typ) {
            (TokenType::Identifier(_), TokenType::Walrus) => {
                self.parse_new_assignment()
            }
            (TokenType::Identifier(_), TokenType::Equals) => {
                self.parse_reassignment()
            }
            _ => {
                self.parse_binary_ops_1()
            }
        }
    }

    fn parse_new_assignment(&mut self) -> Expr {
        let ident = self.parse_full_identifier();
        self.require(TokenType::Walrus);
        let value = self.parse_or();
        Expr::new(ExprKind::NewAssignment(ident, Ptr(value)))
    }

    fn parse_reassignment(&mut self) -> Expr {
        let ident = self.parse_full_identifier();
        self.require(TokenType::Equals);
        let value = self.parse_or();
        Expr::new(ExprKind::Reassignment(ident, Ptr(value)))
    }

    fn parse_binary_ops_1(&mut self) -> Expr {
        let mut expr = self.parse_binary_ops_2();
        while matches!(self.buffer[0].typ, TokenType::Plus | TokenType::Dash) {
            if self.is_token(TokenType::Plus) {
                self.consume();
                let right = self.parse_binary_ops_2();
                expr = Expr::new(ExprKind::BinaryOp(
                    Ptr(expr),
                    Ptr(right),
                    BinaryOp::Addition,
                ));
            } else if self.is_token(TokenType::Dash) {
                self.consume();
                let right = self.parse_binary_ops_2();
                expr = Expr::new(ExprKind::BinaryOp(
                    Ptr(expr),
                    Ptr(right),
                    BinaryOp::Subtraction,
                ));
            }
        }
        expr
    }

    fn parse_binary_ops_2(&mut self) -> Expr {
        let expr = self.parse_innermost_expr();
        while matches!(self.buffer[0].typ, TokenType::Asterisk | TokenType::Slash) {
            /*println!(
                "endless looping on token in bin op 2 {:?}",
                self.buffer[0].typ
            );*/
        }
        return expr;
    }

    /// e.g. references, constants, etc
    fn parse_innermost_expr(&mut self) -> Expr {
        return match &self.buffer[0].typ {
            TokenType::True => {
                let expr = Expr::new(ExprKind::BoolConstant(true));
                self.consume();
                expr
            }
            TokenType::False => {
                let expr = Expr::new(ExprKind::BoolConstant(false));
                self.consume();
                expr
            }
            TokenType::Integer(val) => {
                let expr = Expr::new(ExprKind::IntConstant(*val));
                self.consume();
                expr
            }
            TokenType::String(val) => {
                let expr = Expr::new(ExprKind::StringConstant(val.clone()));
                self.consume();
                expr
            }
            TokenType::Identifier(_) => {
                let ident = self.parse_full_identifier();
                if self.is_token(TokenType::LeftParenthesis) {
                    self.parse_function_call(ident)
                } else {
                    Expr::new(ExprKind::Reference(ident))
                }
            }
            TokenType::LeftParenthesis => {
                self.require(TokenType::LeftParenthesis);
                let expr = self.parse_or();
                self.require(TokenType::RightParenthesis);
                return expr;
            }
            _ => panic!(
                "oh no, unknown token {:?} not parsable as expr",
                self.buffer[0].typ
            ),
        };
    }

    fn parse_function_call(&mut self, ident: String) -> Expr {
        let arguments = self.parse_argument_list();
        Expr::new(ExprKind::FunctionCall(ident, arguments))
    }

    fn parse_argument_list(&mut self) -> Vec<Ptr<Expr>> {
        self.require(TokenType::LeftParenthesis);
        let mut arguments: Vec<Ptr<Expr>> = Vec::new();
        while !self.is_token(TokenType::RightParenthesis) && !self.is_token(TokenType::EndOfText) {
            let arg = self.parse_or();
            arguments.push(Ptr(arg));
            if self.is_token(TokenType::Comma) {
                self.consume();
            }
        }
        self.require(TokenType::RightParenthesis);
        return arguments;
    }

    fn parse_full_identifier(&mut self) -> String {
        // TODO: use this for adding up namespaced identifiers later on
        match &self.buffer[0].typ {
            TokenType::Identifier(val) => {
                let ret = val.clone();
                self.consume();
                ret
            }
            t => panic!("expected identifier, was {:#?}", t)
        }
    }

    fn parse_parameter_list(&mut self) -> Vec<Ptr<(String, String)>> {
        self.require(TokenType::LeftParenthesis);
        let mut params: Vec<Ptr<(String, String)>> = Vec::new();
        while !self.is_token(TokenType::RightParenthesis) && !self.is_token(TokenType::EndOfText) {
            let ident = self.parse_full_identifier();
            self.require(TokenType::Colon);
            let typ = self.parse_full_identifier();
            /*println!(
                "[found arg] {}: {}",
                ident.string.clone().unwrap(),
                typ.string.clone().unwrap()
            );*/
            params.push(Ptr((ident, typ)));
            if self.is_token(TokenType::Comma) {
                self.consume();
            }
        }
        self.require(TokenType::RightParenthesis);
        return params;
    }

    fn is_token(&mut self, token: TokenType) -> bool {
        return self.buffer[0].typ == token;
    }

    fn consume(&mut self) {
        self.buffer.swap(0, 1);
        self.buffer[1] = self.lexer.next_token();
    }

    fn consume_and_return(&mut self) -> Token {
        let consumed = self.buffer[0].clone();
        self.buffer.swap(0, 1);
        self.buffer[1] = self.lexer.next_token();
        return consumed;
    }

    fn require(&mut self, token: TokenType) {
        // check only if the enum variant is the same, we don't care about any value they are holding
        let consumed = self.consume_and_return();
        if consumed.typ != token {
            panic!("oh no, expected {:?} but was {:?}", token, consumed);
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn string_constant() {
        let lexer = Lexer::new("\"hello\"".into());
        let (root, _) = Parser::parse(lexer);
        let main =
            root.functions.iter()
                .map(|f| match &f.kind {
                    ExprKind::Function(f) => f,
                    _ => unreachable!(),
                })
                .filter(|f| f.name == ".main")
                .next()
                .unwrap();

        match main.code.ptr.exprs.first().unwrap().kind {
            ExprKind::StringConstant(_) => {}, // as expected
            _ => assert!(false, "String constant did not parse into a string constant expression AST node"),
        }
    }
}