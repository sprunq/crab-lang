use std::fmt;

use crate::{
    object::object::Object,
    parser::{infix::Infix, prefix::Prefix},
};

#[derive(Debug, Clone, PartialEq)]
pub enum EvalErr {
    CannotApplyPrefix(Prefix, Object),
    IncompatibleTypes(Infix, Object, Object),
    UnsupportedOperand(Infix, Object, Object),
    IdentifierNotFound(String),
    NotCallable(Object),
    WrongArgumentCount(usize, usize),
    UnsupportedHashKey(Object),
    UnknownIndexOperator(Object, Object),
    UnsupportedArguments(String, Vec<Object>),
    UnknownInfixOperator(Infix, Object, Object),
}

impl fmt::Display for EvalErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            EvalErr::CannotApplyPrefix(prefix, obj) => write!(f, "{:?} {:?}", prefix, obj),
            EvalErr::IncompatibleTypes(infix, obj_l, obj_r) => {
                write!(f, "{:?} {:?} {:?}", obj_l, infix, obj_r)
            }
            EvalErr::UnsupportedOperand(infix, obj_l, obj_r) => {
                write!(f, "{:?} {:?} {:?}", obj_l, infix, obj_r)
            }
            EvalErr::IdentifierNotFound(ident) => write!(f, "{:?}", ident),
            EvalErr::NotCallable(obj) => write!(f, "{:?}", obj),
            EvalErr::WrongArgumentCount(expected, actual) => {
                write!(f, "{:?} {:?}", expected, actual)
            }
            EvalErr::UnsupportedHashKey(obj) => write!(f, "{:?}", obj),
            EvalErr::UnknownIndexOperator(l_eval, r_eval) => write!(f, "{:?} {:?}", l_eval, r_eval),
            EvalErr::UnsupportedArguments(name, arguments) => write!(
                f,
                "{:?} {:?}",
                name,
                arguments
                    .iter()
                    .map(|a| a.get_type_name())
                    .collect::<Vec<&str>>()
                    .join(", ")
            ),
            EvalErr::UnknownInfixOperator(infix, left, right) => {
                write!(f, "{:?} {:?} {:?}", left, infix, right)
            }
        }
    }
}
