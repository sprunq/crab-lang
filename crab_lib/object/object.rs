use std::fmt;

use crate::{evaluator::eval_error::EvalErr, parser::prefix::Prefix};

#[derive(PartialEq, Debug, Clone)]
pub enum Object {
    Integer(i128),
    Boolean(bool),
    Null,
    Return(Box<Object>),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Object::Integer(val) => write!(f, "{}", val),
            Object::Boolean(val) => write!(f, "{}", val),
            Object::Null => write!(f, "null"),
            Object::Return(val) => write!(f, "{}", val),
        }
    }
}

impl Object {
    pub fn get_type_name(&self) -> &str {
        match &self {
            Object::Integer(_) => "BOOLEAN",
            Object::Boolean(_) => "INTEGER",
            Object::Null => "NULL",
            Object::Return(_) => "RETURN",
        }
    }

    pub fn is_truthy(&self) -> Result<bool, EvalErr> {
        match self {
            Object::Boolean(value) => Ok(*value),
            Object::Null => Ok(false),
            _ => Err(EvalErr::CannotApplyPrefix(Prefix::Bang, self.clone())),
        }
    }
}
