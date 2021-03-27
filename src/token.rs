#[derive(Debug, Clone)]
pub struct Token {
    pub typ: TokenType,
}


const _EMPTY: Token = Token { // TODO: I'm not sure this does what I intend, look up static/const and determine how to make sure there's only one shared immutable reference to this instance, if I understand this correctly it'll just create a copy for every call anyway..
    typ: TokenType::None,
};

impl Token {
    pub fn new(typ: TokenType) -> Token {
        Token {
            typ,
        }
    }

    pub fn empty() -> Token {
        return _EMPTY;
    }

    pub fn ident(value: String) -> Token {
        Token {
            typ: TokenType::Identifier(value),
        }
    }

    pub fn string(value: String) -> Token {
        Token {
            typ: TokenType::String(value),
        }
    }

    pub fn integer(value: i64) -> Token {
        Token {
            typ: TokenType::Integer(value),
        }
    }

    pub fn double(value: f64) -> Token {
        Token {
            typ: TokenType::Double(value),
        }
    }

    pub fn unknown(value: String) -> Token {
        Token {
            typ: TokenType::Unknown(value),
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
