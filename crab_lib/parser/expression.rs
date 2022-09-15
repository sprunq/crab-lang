use std::fmt;

use super::{infix::Infix, prefix::Prefix, statement::BlockStatement};

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Infix(Infix, Box<Expression>, Box<Expression>),
    Prefix(Prefix, Box<Expression>),
    Identifier(String),
    IntegerLiteral(i128),
    FloatLiteral(f64),
    BooleanLiteral(bool),
    StringLiteral(String),
    If(Box<Expression>, BlockStatement, Option<BlockStatement>),
    Empty,
    FunctionLiteral(Vec<String>, BlockStatement),
    Call(Box<Expression>, Vec<Expression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Infix(op, l, r) => write!(f, "({} {} {})", l, op, r),
            Expression::Identifier(ident) => write!(f, "{}", ident),
            Expression::IntegerLiteral(int) => write!(f, "{}", int),
            Expression::FloatLiteral(float) => write!(f, "{}", float),
            Expression::StringLiteral(s) => write!(f, "\"{}\"", s),
            Expression::Empty => write!(f, ""),
            Expression::Prefix(prefix, expr) => write!(f, "({}{})", prefix, expr),
            Expression::BooleanLiteral(bool) => write!(f, "{}", bool),
            Expression::If(condition, consequence, alternative) => {
                write!(f, "if {} {}", condition, consequence)?;
                if let Some(alt) = alternative {
                    write!(f, " else {}", alt)?;
                }
                Ok(())
            }
            Expression::FunctionLiteral(parameters, body) => {
                write!(f, "function({}) {}", parameters.join(", "), body)
            }
            Expression::Call(function, arguments) => {
                write!(f, "{}({})", function, {
                    arguments
                        .iter()
                        .map(|a| a.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                })
            }
        }
    }
}
