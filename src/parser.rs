use crate::ast::{BinaryOp, Block, Expr, ExprKind, Ptr, Root};
use crate::lexer::Lexer;
use crate::token::*;

pub struct Parser {
    lexer: Lexer,
    buffer: [Token; 2],
    pub root: Root,
}

impl Parser {
    pub fn parse(lexer: Lexer) -> Root {
        let mut parser = Parser {
            lexer,
            buffer: [Token::empty(), Token::empty()],
            root: Root::new(),
        };
        parser.consume();
        parser.consume();
        parser.parse_private();
        return parser.root;
    }

    fn parse_private(&mut self) {
        self.parse_global_scope();
    }

    fn parse_global_scope(&mut self) {
        let mut global_block = Block::new();
        while !self.is_token(TokenType::EndOfText) {
            match self.buffer[0].typ {
                TokenType::Function => {
                    let func = self.parse_global_function();
                    //println!("Added global {:?}", func.kind);
                    self.root.functions.push(func);
                },
                TokenType::Identifier => {
                    let name = self.parse_full_identifier();
                    if !self.is_token(TokenType::LeftParenthesis) {
                        // just to simply for now, only allow function calls in global scope
                        panic!("only function calls are allowed as expressions in global scope, you doofus :)");
                    }

                    let call = self.parse_function_call_in_global_scope(name);
                    //println!("Added global {:?}", call.kind);
                    self.require(TokenType::Semicolon);
                    global_block.exprs.push(call);
                },
                TokenType::Semicolon => self.consume(),
                _ => panic!("I DON'T KNOW WHAT {:?} IS", &self.buffer[0]),
            };
        }
        global_block.exprs.push(Expr::new(ExprKind::Return(None)));
        self.root.functions.push(
            Expr::new(ExprKind::Function(".main".into(), "any".into(), Vec::new(), Ptr(global_block))));
    }

    fn parse_function_call_in_global_scope(&mut self, name: String) -> Expr {
        let arguments = self.parse_argument_list();
        return Expr::new(ExprKind::FunctionCall(name, arguments));
    }

    fn parse_global_function(&mut self) -> Expr {
        self.require(TokenType::Function);
        let ident = self.require_and_return(TokenType::Identifier);
        //println!("function identifier: {}", ident.string.clone().unwrap());
        let parameters = self.parse_parameter_list();
        self.require(TokenType::Arrow);
        let return_type = self.require_and_return(TokenType::Identifier);
        /*println!(
            "function ident: '{}', function return type: '{}'",
            ident.string.clone().unwrap(),
            return_type.string.clone().unwrap()
        );*/
        let block = self.parse_block();
        return Expr::new(ExprKind::Function(
            ident.string.unwrap(),
            return_type.string.unwrap(),
            parameters,
            block,
        ));
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
                let expr = self.parse_outermost_expr();
                self.require(TokenType::Semicolon);
                return Expr::new(ExprKind::Return(Some(Ptr(expr))));
            }
            _ => {
                let expr = self.parse_outermost_expr();
                return expr;
            }
        }
    }

    /// e.g. binary ops, etc
    fn parse_outermost_expr(&mut self) -> Expr {
        return self.parse_binary_ops_1();
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
        return expr;
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
        return match self.buffer[0].typ {
            TokenType::Integer => Expr::new(ExprKind::IntConstant(
                self.consume_and_return().integer.unwrap(),
            )),
            TokenType::String => Expr::new(ExprKind::StringConstant(self.consume_and_return().string.unwrap())),
            TokenType::Identifier => {
                let ident = self.parse_full_identifier();
                if self.is_token(TokenType::LeftParenthesis) {
                    return self.parse_function_call(ident);
                } else {
                    return Expr::new(ExprKind::Reference(ident));
                }
            },
            TokenType::LeftParenthesis => {
                self.require(TokenType::LeftParenthesis);
                let expr = self.parse_outermost_expr();
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
        return Expr::new(ExprKind::FunctionCall(ident, arguments));
    }

    fn parse_argument_list(&mut self) -> Vec<Ptr<Expr>> {
        self.require(TokenType::LeftParenthesis);
        let mut arguments : Vec<Ptr<Expr>> = Vec::new();
        while !self.is_token(TokenType::RightParenthesis) && !self.is_token(TokenType::EndOfText) {
            let arg = self.parse_outermost_expr();
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
        return self.consume_and_return().string.clone().unwrap();
    }

    fn parse_parameter_list(&mut self) -> Vec<Ptr<(String, String)>> {
        self.require(TokenType::LeftParenthesis);
        let mut params: Vec<Ptr<(String, String)>> = Vec::new();
        while !self.is_token(TokenType::RightParenthesis) && !self.is_token(TokenType::EndOfText) {
            let ident = self.require_and_return(TokenType::Identifier);
            self.require(TokenType::Colon);
            let typ = self.require_and_return(TokenType::Identifier);
            /*println!(
                "[found arg] {}: {}",
                ident.string.clone().unwrap(),
                typ.string.clone().unwrap()
            );*/
            params.push(Ptr((ident.string.unwrap(), typ.string.unwrap())));
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
            panic!("oh no, expected {:?} but was {:?}", token, consumed.typ);
        }
    }

    fn require_and_return(&mut self, token: TokenType) -> Token {
        let consumed = self.consume_and_return();
        // check only if the enum variant is the same, we don't care about any value they are holding
        if consumed.typ != token {
            panic!("oh no, expected {:?} but was {:?}", token, consumed.typ);
        }
        return consumed;
    }
}
