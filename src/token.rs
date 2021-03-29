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
#[derive(Debug, Clone)]
pub struct Token {
    pub typ: TokenType,
    pub line: u64,
    pub col: u64,
}


const _EMPTY: Token = Token { // TODO: I'm not sure this does what I intend, look up static/const and determine how to make sure there's only one shared immutable reference to this instance, if I understand this correctly it'll just create a copy for every call anyway..
    typ: TokenType::None,
    line: 0,
    col: 0,
};

impl Token {
    pub fn new(typ: TokenType) -> Token {
        Token {
            typ,
            line: 0,
            col: 0
        }
    }

    pub fn with_pos(typ: TokenType, line: u64, col: u64) -> Token {
        Token {
            typ,
            line,
            col
        }
    }

    pub fn empty() -> Token {
        return _EMPTY;
    }

    pub fn ident(value: String) -> Token {
        Token {
            typ: TokenType::Identifier(value),
            line: 0,
            col: 0,
        }
    }

    pub fn string(value: String) -> Token {
        Token {
            typ: TokenType::String(value),
            line: 0,
            col: 0,
        }
    }

    pub fn integer(value: i64) -> Token {
        Token {
            typ: TokenType::Integer(value),
            line: 0,
            col: 0,
        }
    }

    pub fn double(value: f64) -> Token {
        Token {
            typ: TokenType::Double(value),
            line: 0,
            col: 0,
        }
    }

    pub fn unknown(value: String) -> Token {
        Token {
            typ: TokenType::Unknown(value),
            line: 0,
            col: 0,
        }
    }

    pub fn print(self) {
        println!("{:?}", self);
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    None,
    Unknown(String),
    Identifier(String),
    EndOfText,
    /* Start Keywords */
    Function,
    If,
    Else,
    Return,
    /* Start Symbols */
    Comma,
    Dot,
    Plus,
    Dash,
    Colon,
    DoubleColon,
    Walrus,
    Semicolon,
    LessThan,
    GreaterThan,
    LessThanEquals,
    GreaterThanEquals,
    Equals,
    EqualsEquals,
    NotEquals,
    And,
    Or,
    Asterisk,
    Slash,
    Arrow,
    LeftParenthesis,
    RightParenthesis,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    /* Start types */
    String(String),
    Integer(i64),
    Double(f64),
}
