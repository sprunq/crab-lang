use crate::token::Token;
use std::fmt;

#[derive(Debug)]
pub enum ParseErr {
    ExpectedPrefixToken(Token),
    ExpectedInfixToken(Token),
    ExpectedIdentifierToken(Token),
    ExpectedBoolToken(Token),
    ExpectedIntegerToken(Token),
    ExpectedFloatToken(Token),
    ExpectedStringToken(Token),
    ExpectedLparen(Token),
    ExpectedRparen(Token),
    ExpectedLbrace(Token),
    ExpectedRbrace(Token),
    ExpectedRbracket(Token),
    ExpectedAssign(Token),
    ExpectedSemicolon(Token),
    ExpectedComma(Token),
    ExpectedColon(Token),
    ParseInt(String),
    ParseFloat(String),
}

impl fmt::Display for ParseErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            ParseErr::ExpectedPrefixToken(val) => write!(f, "{}", format_token("Prefix", val)),
            ParseErr::ExpectedInfixToken(val) => write!(f, "{}", format_token("Infix", val)),
            ParseErr::ExpectedIdentifierToken(val) => {
                write!(f, "{}", format_token("Indetifier", val))
            }
            ParseErr::ExpectedBoolToken(val) => write!(f, "{}", format_token("Boolean", val)),
            ParseErr::ExpectedIntegerToken(val) => {
                write!(f, "{}", format_token("Integer", val))
            }
            ParseErr::ExpectedFloatToken(val) => write!(f, "{}", format_token("Float", val)),
            ParseErr::ExpectedStringToken(val) => write!(f, "{}", format_token("String", val)),
            ParseErr::ExpectedLparen(val) => write!(f, "{}", format_token("LParen", val)),
            ParseErr::ExpectedRparen(val) => write!(f, "{}", format_token("RParen", val)),
            ParseErr::ExpectedLbrace(val) => write!(f, "{}", format_token("LBrace", val)),
            ParseErr::ExpectedRbrace(val) => write!(f, "{}", format_token("RBrace", val)),
            ParseErr::ExpectedRbracket(val) => write!(f, "{}", format_token("RBracket", val)),
            ParseErr::ExpectedAssign(val) => write!(f, "{}", format_token("Assign", val)),
            ParseErr::ExpectedSemicolon(val) => write!(f, "{}", format_token("Semicolon", val)),
            ParseErr::ExpectedComma(val) => write!(f, "{}", format_token("Comma", val)),
            ParseErr::ExpectedColon(val) => write!(f, "{}", format_token("Colon", val)),
            ParseErr::ParseInt(val) => write!(f, "{}", format_str("Int", val)),
            ParseErr::ParseFloat(val) => write!(f, "{}", format_str("Float", val)),
        }
    }
}
fn format_token(expected_token: &str, error_token: &Token) -> String {
    format!("Expected '{}' but got '{:?}'", expected_token, error_token)
}

fn format_str(expected_token: &str, error_token: &str) -> String {
    format!("Expected '{}' but got '{:?}'", expected_token, error_token)
}
