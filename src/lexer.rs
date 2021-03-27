use crate::token::*;
use std::collections::HashMap;
use std::iter::FromIterator;
use std::convert::TryFrom;

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
    // TODO: could wrap the mutable parts in a mutable wrapper, but meh.. maybe later
    code: String,
    buffer: [char; 2],
    pos: i64,
    line: i64,
    col: i64,
    pub eof: bool,
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
            keywords: keywords! [
                "fn" => TokenType::Function,
                "if" => TokenType::If,
                "else" => TokenType::Else,
                "return" => TokenType::Return
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
        if self.eof {
            return Token::new(TokenType::EndOfText);
        }
        let mut tok = Token::empty();
        while tok.typ == TokenType::None && !self.eof {
            match self.buffer[0] {
                '\n' | ' ' => (),
                '(' => tok = Token::new(TokenType::LeftParenthesis),
                ')' => tok = Token::new(TokenType::RightParenthesis),
                ',' => tok = Token::new(TokenType::Comma),
                '.' => tok = Token::new(TokenType::Dot),
                '{' => tok = Token::new(TokenType::LeftBrace),
                '}' => tok = Token::new(TokenType::RightBrace),
                '[' => tok = Token::new(TokenType::LeftBracket),
                ']' => tok = Token::new(TokenType::RightBracket),
                ':' => match self.buffer[1] {
                    ':' => {
                        tok = Token::new(TokenType::DoubleColon);
                        self.consume();
                    },
                    _ => tok = Token::new(TokenType::Colon),
                },
                ';' => tok = Token::new(TokenType::Semicolon),
                '<' => match self.buffer[1] {
                    '=' => {
                        tok = Token::new(TokenType::LessThanEquals);
                        self.consume();
                    },
                    _ => tok = Token::new(TokenType::LessThan),
                }
                '>' => match self.buffer[1] {
                    '=' => {
                        tok = Token::new(TokenType::GreaterThanEquals);
                        self.consume();
                    },
                    _ => tok = Token::new(TokenType::GreaterThan),
                }
                '&' => match self.buffer[1] {
                    '&' => {
                        tok = Token::new(TokenType::And);
                        self.consume();
                    },
                    _ => panic!("binary and op '&' not implemented"),
                },
                '|' => match self.buffer[1] {
                    '|' => {
                        tok = Token::new(TokenType::Or);
                        self.consume();
                    },
                    _ => panic!("binary or op '|' not implemented"),
                },
                '!' => match self.buffer[1] {
                    '=' => {
                        tok = Token::new(TokenType::NotEquals);
                        self.consume();
                    },
                    _ => panic!("unary not '!' not implemented"),
                },
                '=' => match self.buffer[1] {
                    '=' => {
                        tok = Token::new(TokenType::EqualsEquals);
                        self.consume();
                    },
                    _ => tok = Token::new(TokenType::Equals),
                }
                '*' => tok = Token::new(TokenType::Asterisk),
                '/' => match self.buffer[1] {
                    '/' => {
                        while self.buffer[0] != '\n' && !self.eof {
                            self.consume();
                        }
                    },
                    '*' => {
                        while (self.buffer[0] != '*' || self.buffer[1] != '/') && !self.eof {
                            self.consume();
                        }
                        self.consume(); // consume last '/'
                    },
                    _ => tok = Token::new(TokenType::Slash),
                },
                '+' => tok = Token::new(TokenType::Plus),
                '-' => match self.buffer[1] {
                    '0' ..= '9' => return self.get_number(),
                    '>' => {
                        self.consume();
                        tok = Token::new(TokenType::Arrow)
                    },
                    _ => tok = Token::new(TokenType::Dash),
                }
                ,
                '0' ..= '9' => return self.get_number(),
                '"' => return self.get_string(),
                'A' ..= 'Z' | 'a' ..= 'z' | '_' => return self.get_identifier_or_keyword(),
                _ => tok = Token::unknown(String::from(format!("b[0] = {} --- b[1] = {}", self.buffer[0], self.buffer[1]))),
            };

            self.consume();
            if self.eof {
                return Token::new(TokenType::EndOfText);
            }
        }
        return tok;
    }

    fn get_number(&mut self) -> Token {
        let mut sb = Vec::new();
        sb.push(self.buffer[0]); // add first (no need to check for dash)
        self.consume();
        while matches!(self.buffer[0], '0' ..= '9') {
            sb.push(self.buffer[0]);
            self.consume();
        }
        let n = String::from_iter(sb).parse::<i64>();

        return Token::integer(n.unwrap());
    }

    fn get_identifier_or_keyword(&mut self) -> Token {
        let mut sb = Vec::new();
        sb.push(self.buffer[0]); // add first
        self.consume();
        while matches!(self.buffer[0], 
            'A' ..= 'Z'
            | 'a' ..= 'z'
            | '0' ..= '9'
            | '_') {
            sb.push(self.buffer[0]);
            self.consume();
        }
        let s = String::from_iter(sb);
        return match self.keywords.get(&s) {
            Some(kw) => Token::new(kw.clone()),
            None => Token::ident( s),
        };
    }

    fn get_string(&mut self) -> Token {
        self.consume(); // skip "
        if self.buffer[0] == '"' {
            self.consume(); // skip "
            return Token::string(String::from(""));
        }

        let mut sb = Vec::new();
        while self.buffer[0] != '"' {
            match self.buffer[0] {
                '\\' => match self.buffer[1] {
                    '"' => {
                        self.consume();
                        sb.push(self.buffer[0]);
                    }
                    'n' => {
                        self.consume();
                        sb.push('\n');
                    }
                    't' => {
                        self.consume();
                        sb.push('\t');
                    }
                    '\\' => {
                        self.consume();
                        sb.push('\\');
                    }
                    _ => sb.push(self.buffer[0])
                },
                _ => sb.push(self.buffer[0])
            }
            self.consume();
        }
        self.consume(); // consume last "
        return Token::string(String::from_iter(sb));
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

        if self.pos == i64::try_from(self.code.len()).unwrap() {
            // TODO: use match and do /\ this in a more safe way, also store the length as i64 to not have to convert it all the time
            self.buffer[1] = '\n';
            self.pos += 1;
        } else if self.pos > i64::try_from(self.code.len()).unwrap() {
            self.eof = true;
        } else {
            self.buffer[1] = self.code.as_bytes()[self.pos as usize] as char;
            self.pos += 1;
            self.col += 1;
        }
    }
}
