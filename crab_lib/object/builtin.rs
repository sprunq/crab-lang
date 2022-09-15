// Builtin functions must have a static order in order to have static indices in compiled bytecodes.

use crate::evaluator::eval_error::EvalErr;

use super::object::Object;

pub struct Builtin {
    pub name: &'static str,
    pub builtin: Object,
}

macro_rules! builtin {
    ($name:ident) => {
        Builtin {
            name: stringify!($name),
            builtin: Object::Builtin($name),
        }
    };
}

pub const BUILTINS: &[Builtin] = &[builtin!(print), builtin!(exit)];

pub fn lookup(name: &str) -> Option<Object> {
    if name == "null" {
        return Some(Object::Null);
    }
    for b in BUILTINS {
        if b.name == name {
            return Some(b.builtin.clone());
        }
    }
    None
}

fn print(arguments: Vec<Object>) -> Result<Object, EvalErr> {
    for arg in arguments {
        println!("{}", arg);
    }
    Ok(Object::Null)
}

fn exit(_arguments: Vec<Object>) -> Result<Object, EvalErr> {
    std::process::exit(0);
}
