#[derive(Debug, Clone)]
pub struct Token {
    pub typ: TokenType,
    pub string: Option<String>,
    pub integer: Option<i64>,
    pub double: Option<f64>,
}


#[allow(non_snake_case)]
pub fn Token(typ: TokenType) -> Token {
    Token {
        typ,
        string: None,
        integer: None,
        double: None,
    }
}

const _EMPTY: Token = Token { // TODO: I'm not sure this does what I intend, look up static/const and determine how to make sure there's only one shared immutable reference to this instance, if I understand this correctly it'll just create a copy for every call anyway..
    typ: TokenType::None,
    string: None,
    integer: None,
    double: None,
};

impl Token {
    pub fn empty() -> Token {
        return _EMPTY;
    }

    pub fn with_string(typ: TokenType, value: String) -> Token {
        Token {
            typ,
            string: Some(value),
            integer: None,
            double: None,
        }
    }

    pub fn with_integer(typ: TokenType, value: i64) -> Token {
        Token {
            typ,
            string: None,
            integer: Some(value),
            double: None,
        }
    }

    pub fn with_double(typ: TokenType, value: f64) -> Token {
        Token {
            typ,
            string: None,
            integer: None,
            double: Some(value),
        }
    }

    pub fn print(self) {
        println!("{:?}", self);
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    None,
    Unknown,
    Identifier,
    EndOfText,
    /* Start Keywords */
    Function,
    If,
    Return,
    /* Start Symbols */
    Comma,
    Dot,
    Plus,
    Dash,
    Colon,
    DoubleColon,
    Semicolon,
    LessThan,
    GreaterThan,
    Equals,
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
    String,
    Integer,
    Double,
}
