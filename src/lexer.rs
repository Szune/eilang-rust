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
use crate::token::*;
use crate::types::TypeCollector;
use std::collections::{HashMap, VecDeque};
use std::iter::FromIterator;

macro_rules! keywords (
    ($($str:expr => $token:expr),*) => {
        [
            $(
                (String::from($str), $token),
            )*
        ]
    }
);

pub struct Lexer {
    code: String,
    buffer: [char; 2],
    pos: usize,
    line: u64,
    col: u64,
    pub eof: bool,
    queue: VecDeque<Token>,
    keywords: HashMap<String, TokenType>,
}

impl Lexer {
    pub fn new(code: String) -> Lexer {
        let mut lexer = Lexer {
            code,
            buffer: [' ', ' '],
            pos: 0,
            line: 0,
            col: 0,
            eof: false,
            queue: VecDeque::new(),
            keywords: keywords![
                "fn" => TokenType::Function,
                "if" => TokenType::If,
                "else" => TokenType::Else,
                "return" => TokenType::Return,
                "true" => TokenType::True,
                "false" => TokenType::False
            ]
            .iter()
            .cloned()
            .collect(),
        };
        lexer.consume();
        lexer.consume();
        return lexer;
    }

    pub fn next_token(&mut self) -> Token {
        if !self.queue.is_empty() {
            self.queue.pop_front().unwrap()
        } else {
            self.next_token_no_queue()
        }
    }

    fn next_token_no_queue(&mut self) -> Token {
        macro_rules! Token (
            ($t:path) => {
                Token::with_pos($t, self.line, self.col);
            }
        );
        if self.eof {
            return Token!(TokenType::EndOfText);
        }
        let mut tok = Token::empty();
        while tok.typ == TokenType::None && !self.eof {
            match self.buffer[0] {
                '\n' | ' ' => (),
                '$' => match self.buffer[1] {
                    '"' => {
                        self.queue_interpolated_string();
                        return self.queue.pop_front().unwrap();
                    }
                    _ => unimplemented!(),
                },
                '(' => tok = Token!(TokenType::LeftParenthesis),
                ')' => tok = Token!(TokenType::RightParenthesis),
                ',' => tok = Token!(TokenType::Comma),
                '.' => tok = Token!(TokenType::Dot),
                '{' => tok = Token!(TokenType::LeftBrace),
                '}' => tok = Token!(TokenType::RightBrace),
                '[' => tok = Token!(TokenType::LeftBracket),
                ']' => tok = Token!(TokenType::RightBracket),
                ':' => match self.buffer[1] {
                    ':' => {
                        tok = Token!(TokenType::DoubleColon);
                        self.consume();
                    }
                    '=' => {
                        tok = Token!(TokenType::Walrus);
                        self.consume();
                    }
                    _ => tok = Token!(TokenType::Colon),
                },
                ';' => tok = Token!(TokenType::Semicolon),
                '<' => match self.buffer[1] {
                    '=' => {
                        tok = Token!(TokenType::LessThanEquals);
                        self.consume();
                    }
                    _ => tok = Token!(TokenType::LessThan),
                },
                '>' => match self.buffer[1] {
                    '=' => {
                        tok = Token!(TokenType::GreaterThanEquals);
                        self.consume();
                    }
                    _ => tok = Token!(TokenType::GreaterThan),
                },
                '&' => match self.buffer[1] {
                    '&' => {
                        tok = Token!(TokenType::And);
                        self.consume();
                    }
                    _ => panic!("binary and op '&' not implemented"),
                },
                '|' => match self.buffer[1] {
                    '|' => {
                        tok = Token!(TokenType::Or);
                        self.consume();
                    }
                    _ => panic!("binary or op '|' not implemented"),
                },
                '!' => match self.buffer[1] {
                    '=' => {
                        tok = Token!(TokenType::NotEquals);
                        self.consume();
                    }
                    _ => panic!("unary not '!' not implemented"),
                },
                '=' => match self.buffer[1] {
                    '=' => {
                        tok = Token!(TokenType::EqualsEquals);
                        self.consume();
                    }
                    _ => tok = Token!(TokenType::Equals),
                },
                '*' => tok = Token!(TokenType::Asterisk),
                '/' => match self.buffer[1] {
                    '/' => {
                        while self.buffer[0] != '\n' && !self.eof {
                            self.consume();
                        }
                    }
                    '*' => {
                        while (self.buffer[0] != '*' || self.buffer[1] != '/') && !self.eof {
                            self.consume();
                        }
                        self.consume(); // consume last '/'
                    }
                    _ => tok = Token!(TokenType::Slash),
                },
                '+' => tok = Token!(TokenType::Plus),
                '-' => match self.buffer[1] {
                    '0'..='9' => return self.get_number(),
                    '>' => {
                        self.consume();
                        tok = Token!(TokenType::Arrow)
                    }
                    _ => tok = Token!(TokenType::Dash),
                },
                '0'..='9' => return self.get_number(),
                '"' => return self.get_string(),
                'A'..='Z' | 'a'..='z' | '_' => return self.get_identifier_or_keyword(),
                _ => {
                    let unknown = TokenType::Unknown(String::from(format!(
                        "b[0] = {} --- b[1] = {}",
                        self.buffer[0], self.buffer[1]
                    )));
                    tok = Token!(unknown);
                }
            };

            self.consume();
            if self.eof && tok.typ == TokenType::None {
                return Token!(TokenType::EndOfText);
            }
        }
        return tok;
    }

    fn get_number(&mut self) -> Token {
        let mut sb = Vec::new();
        sb.push(self.buffer[0]); // add first (no need to check for dash)
        self.consume();
        while matches!(self.buffer[0], '0'..='9') {
            sb.push(self.buffer[0]);
            self.consume();
        }
        let n = String::from_iter(sb).parse::<i64>();

        return Token::with_pos(TokenType::Integer(n.unwrap()), self.line, self.col);
    }

    fn get_identifier_or_keyword(&mut self) -> Token {
        let mut sb = Vec::new();
        sb.push(self.buffer[0]); // add first
        self.consume();
        while matches!(self.buffer[0], 'A' ..= 'Z' | 'a' ..= 'z' | '0' ..= '9' | '_') {
            sb.push(self.buffer[0]);
            self.consume();
        }
        let s = String::from_iter(sb);
        return match self.keywords.get(&s) {
            Some(kw) => Token::with_pos(kw.clone(), self.line, self.col),
            None => self.replace_builtin_aliases(s),
        };
    }

    fn replace_builtin_aliases(&mut self, ident: String) -> Token {
        Token::with_pos(
            TokenType::Identifier(match ident.as_str() {
                "string" => TypeCollector::STRING.into(),
                "bool" => TypeCollector::BOOLEAN.into(),
                "int" => TypeCollector::INT64.into(),
                "double" => TypeCollector::DOUBLE.into(),
                "unit" => TypeCollector::UNIT.into(),
                "void" => TypeCollector::VOID.into(),
                "any" => TypeCollector::ANY.into(),
                _ => ident,
            }),
            self.line,
            self.col,
        )
    }

    fn get_string(&mut self) -> Token {
        self.consume(); // skip "
        if self.buffer[0] == '"' {
            self.consume(); // skip "
            return Token::with_pos(TokenType::String(String::from("")), self.line, self.col);
        }
        let line = self.line;
        let col = self.col;

        let mut sb = Vec::new();
        while self.buffer[0] != '"' {
            if self.eof {
                panic!("Unterminated string on line {} col {}", line, col);
            }
            match (self.buffer[0], self.buffer[1]) {
                ('\\', '"') => {
                    self.consume();
                    sb.push('"');
                }
                ('\\', 'n') => {
                    self.consume();
                    sb.push('\n');
                }
                ('\\', 't') => {
                    self.consume();
                    sb.push('\t');
                }
                ('\\', '\\') => {
                    self.consume();
                    sb.push('\\');
                }
                (_, _) => sb.push(self.buffer[0]),
            }
            self.consume();
        }
        self.consume(); // consume last "
        return Token::with_pos(
            TokenType::String(String::from_iter(sb)),
            self.line,
            self.col,
        );
    }

    fn queue_interpolated_string(&mut self) {
        self.consume(); // $
        self.consume(); // "
        let line = self.line;
        let col = self.col;

        let mut sb = Vec::new();
        while self.buffer[0] != '"' {
            if self.eof {
                panic!(
                    "Unterminated interpolated string on line {} col {}",
                    line, col
                );
            }
            match (self.buffer[0], self.buffer[1]) {
                /* escaped chars */
                ('\\', '"') => {
                    self.consume();
                    sb.push('"');
                }
                ('\\', 'n') => {
                    self.consume();
                    sb.push('\n');
                }
                ('\\', 't') => {
                    self.consume();
                    sb.push('\t');
                }
                ('\\', '\\') => {
                    self.consume();
                    sb.push('\\');
                }
                /* braces */
                ('}', '}') => {
                    self.consume();
                    sb.push('}');
                }
                ('}', _) => panic!(
                    "{}",
                    "Invalid state of interpolated string, '}' found before '{'"
                ),
                ('{', '{') => {
                    self.consume();
                    sb.push('{');
                }
                /* interpolation */
                ('{', _) => {
                    // interpolated tokens here
                    if !self.queue.is_empty() {
                        self.queue
                            .push_back(Token::with_pos(TokenType::Plus, self.line, self.col));
                    }
                    self.queue.push_back(Token::with_pos(
                        TokenType::String(String::from_iter(sb)),
                        self.line,
                        self.col,
                    ));
                    self.queue
                        .push_back(Token::with_pos(TokenType::Plus, self.line, self.col));
                    // implicitly put parentheses around value
                    self.queue.push_back(Token::with_pos(
                        TokenType::LeftParenthesis,
                        self.line,
                        self.col,
                    ));
                    sb = Vec::new();
                    self.consume();

                    while self.buffer[0] != '}' && !self.eof {
                        let next_token = self.next_token_no_queue();
                        self.queue.push_back(next_token);
                    }
                    self.queue.push_back(Token::with_pos(
                        TokenType::RightParenthesis,
                        self.line,
                        self.col,
                    ));
                }
                /* any char */
                (_, _) => sb.push(self.buffer[0]),
            }

            // consume char
            self.consume();
        }

        if self.queue.is_empty() {
            self.queue.push_back(Token::with_pos(
                TokenType::String(String::from_iter(sb)),
                line,
                col,
            ));
            self.consume(); // "
            return;
        }

        if self.queue.back().unwrap().typ != TokenType::Plus {
            self.queue
                .push_back(Token::with_pos(TokenType::Plus, self.line, self.col));
        }
        self.queue.push_back(Token::with_pos(
            TokenType::String(String::from_iter(sb)),
            self.line,
            self.col,
        ));

        self.consume(); // "
    }

    pub fn print_lex(&mut self) {
        println!("Code: {}", self.code);
    }

    fn consume(&mut self) {
        if !self.eof && self.buffer[0] == '\n' {
            self.line += 1;
            self.col = 0;
        }
        self.buffer[0] = self.buffer[1];

        if self.pos == self.code.len() {
            self.buffer[1] = '\n';
            self.pos += 1;
        } else if self.pos > self.code.len() {
            self.eof = true;
        } else {
            self.buffer[1] = self.code.as_bytes()[self.pos] as char;
            self.pos += 1;
            self.col += 1;
        }
    }
}
