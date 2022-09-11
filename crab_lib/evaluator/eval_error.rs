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
        }
    }
}
