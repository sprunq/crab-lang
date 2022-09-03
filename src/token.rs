use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Illegal,
    Eof,
    Identifier(String),
    Int(String),
    Comma,
    Semicolon,
    LParenthesis,
    RParenthesis,
    LBrace,
    RBrace,
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Equal,
    NotEqual,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Token::Illegal => write!(f, "ILLEGAL"),
            Token::Eof => write!(f, "EOF"),
            Token::Identifier(ident) => write!(f, "{}", ident),
            Token::Int(int) => write!(f, "{}", int),
            Token::Assign => write!(f, "="),
            Token::Plus => write!(f, "+"),
            Token::Comma => write!(f, ","),
            Token::Semicolon => write!(f, ";"),
            Token::LParenthesis => write!(f, "("),
            Token::RParenthesis => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::Function => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::Minus => write!(f, "-"),
            Token::Bang => write!(f, "!"),
            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Lt => write!(f, "<"),
            Token::Gt => write!(f, ">"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Return => write!(f, "return"),
            Token::Equal => write!(f, "=="),
            Token::NotEqual => write!(f, "!="),
        }
    }
}

pub fn lookup_ident(ident: &str) -> Token {
    let token = match ident {
        "fn" => Some(Token::Function),
        "let" => Some(Token::Let),
        "true" => Some(Token::True),
        "false" => Some(Token::False),
        "if" => Some(Token::If),
        "else" => Some(Token::Else),
        "return" => Some(Token::Return),
        _ => None,
    };
    if let Some(token) = token {
        token
    } else {
        Token::Identifier(ident.to_owned())
    }
}
