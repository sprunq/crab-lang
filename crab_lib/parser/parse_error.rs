use std::fmt;

use crate::lexer::{lexer::Position, token::Token};

#[derive(Debug, Clone)]
pub enum ParseErr {
    ExpectedPrefixToken(Token, Position),
    ExpectedInfixToken(Token, Position),
    ExpectedIdentifierToken(Token, Position),
    ExpectedBoolToken(Token, Position),
    ExpectedIntegerToken(Token, Position),
    ExpectedFloatToken(Token, Position),
    ExpectedStringToken(Token, Position),
    ExpectedLparen(Token, Position),
    ExpectedRparen(Token, Position),
    ExpectedLbrace(Token, Position),
    ExpectedRbrace(Token, Position),
    ExpectedRbracket(Token, Position),
    ExpectedAssign(Token, Position),
    ExpectedSemicolon(Token, Position),
    ExpectedComma(Token, Position),
    ExpectedColon(Token, Position),
    ParseInt(String, Position),
    ParseFloat(String, Position),
    UnsupportedInfixToken(Token, Position),
}

impl fmt::Display for ParseErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            ParseErr::ExpectedPrefixToken(val, pos) => {
                write!(f, "{}", format_token("Prefix", val, pos))
            }
            ParseErr::ExpectedInfixToken(val, pos) => {
                write!(f, "{}", format_token("Infix", val, pos))
            }
            ParseErr::ExpectedIdentifierToken(val, pos) => {
                write!(f, "{}", format_token("Indetifier", val, pos))
            }
            ParseErr::ExpectedBoolToken(val, pos) => {
                write!(f, "{}", format_token("Boolean", val, pos))
            }
            ParseErr::ExpectedIntegerToken(val, pos) => {
                write!(f, "{}", format_token("Integer", val, pos))
            }
            ParseErr::ExpectedFloatToken(val, pos) => {
                write!(f, "{}", format_token("Float", val, pos))
            }
            ParseErr::ExpectedStringToken(val, pos) => {
                write!(f, "{}", format_token("String", val, pos))
            }
            ParseErr::ExpectedLparen(val, pos) => write!(f, "{}", format_token("LParen", val, pos)),
            ParseErr::ExpectedRparen(val, pos) => write!(f, "{}", format_token("RParen", val, pos)),
            ParseErr::ExpectedLbrace(val, pos) => write!(f, "{}", format_token("LBrace", val, pos)),
            ParseErr::ExpectedRbrace(val, pos) => write!(f, "{}", format_token("RBrace", val, pos)),
            ParseErr::ExpectedRbracket(val, pos) => {
                write!(f, "{}", format_token("RBracket", val, pos))
            }
            ParseErr::ExpectedAssign(val, pos) => write!(f, "{}", format_token("Assign", val, pos)),
            ParseErr::ExpectedSemicolon(val, pos) => {
                write!(f, "{}", format_token("Semicolon", val, pos))
            }
            ParseErr::ExpectedComma(val, pos) => write!(f, "{}", format_token("Comma", val, pos)),
            ParseErr::ExpectedColon(val, pos) => write!(f, "{}", format_token("Colon", val, pos)),
            ParseErr::ParseInt(val, pos) => write!(f, "{}", format_str("Int", val, pos)),
            ParseErr::ParseFloat(val, pos) => write!(f, "{}", format_str("Float", val, pos)),
            ParseErr::UnsupportedInfixToken(val, pos) => {
                write!(f, "{}", format_token("=,+=,*=,/=", val, pos))
            }
        }
    }
}

fn format_token(expected_token: &str, error_token: &Token, pos: &Position) -> String {
    format!(
        "Expected '{}' but got '{:?}' at position {}:{}",
        expected_token, error_token, pos.line, pos.column
    )
}

fn format_str(expected_token: &str, error_token: &str, pos: &Position) -> String {
    format!(
        "Expected '{}' but got '{:?}'at position {}:{}",
        expected_token, error_token, pos.line, pos.column
    )
}
