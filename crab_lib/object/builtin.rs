use super::object::Object;
use crate::evaluator::eval_error::EvalErr;

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

pub const BUILTIN_FUNCTIONS: &[Builtin] = &[builtin!(print), builtin!(sqrt), builtin!(exit)];

pub fn lookup_fn(name: &str) -> Option<Object> {
    if name == "null" {
        return Some(Object::Null);
    }
    for b in BUILTIN_FUNCTIONS {
        if b.name == name {
            return Some(b.builtin.clone());
        }
    }
    None
}

fn print(args: Vec<Object>) -> Result<Object, EvalErr> {
    let mut str = "".to_string();
    for arg in args {
        let s;
        if let Object::String(val) = arg {
            s = str::replace(&val, "\\n", "\n");
        } else {
            s = format!("{}", arg).to_string();
        }
        str = [str, s].concat();
    }
    print!("{}", str);
    Ok(Object::Null)
}

fn sqrt(args: Vec<Object>) -> Result<Object, EvalErr> {
    if args.len() != 1 {
        return Err(EvalErr::WrongArgumentCount(1, 0));
    }
    let arg = args.first().unwrap_or(&Object::Null);
    let input = match arg {
        Object::Integer(val) => *val as f64,
        Object::Float(val) => *val,
        _ => Err(EvalErr::CannotPerformOperation(
            "srqt".to_string(),
            arg.clone(),
        ))?,
    };
    let sqrt = input.sqrt();
    Ok(Object::Float(sqrt))
}

fn exit(_: Vec<Object>) -> Result<Object, EvalErr> {
    std::process::exit(0);
}
