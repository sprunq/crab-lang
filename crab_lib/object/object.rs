use std::{cell::RefCell, fmt, rc::Rc};

use crate::{
    evaluator::eval_error::EvalErr,
    parser::{prefix::Prefix, statement::BlockStatement},
};

use super::environment::Environment;

#[derive(PartialEq, Debug, Clone)]
pub enum Object {
    Boolean(bool),
    Integer(i128),
    Float(f64),
    String(String),
    Null,
    Return(Box<Object>),
    Function(Vec<String>, BlockStatement, Rc<RefCell<Environment>>),
    Builtin(fn(Vec<Object>) -> Result<Object, EvalErr>),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Boolean(value) => write!(f, "{}", value),
            Object::Integer(value) => write!(f, "{}", value),
            Object::Float(value) => write!(f, "{}", value),
            Object::String(value) => write!(f, "{}", value),
            Object::Null => write!(f, "null"),
            Object::Return(value) => write!(f, "{}", *value),
            Object::Function(params, body, _) => {
                write!(f, "fn({}) {{\n{}\n}}", params.join(", "), body)
            }
            Object::Builtin(_) => write!(f, "builtin function"),
        }
    }
}

impl Object {
    pub fn get_type_name(&self) -> &str {
        match &self {
            Object::Boolean(_) => "BOOLEAN",
            Object::Integer(_) => "INTEGER",
            Object::Float(_) => "FLOAT",
            Object::String(_) => "STRING",
            Object::Null => "NULL",
            Object::Return(_) => "RETURN",
            Object::Function(_, _, _) => "FUNCTION",
            Object::Builtin(_) => "BUILTIN",
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
