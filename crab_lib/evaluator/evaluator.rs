use std::{cell::RefCell, rc::Rc};

use crate::{
    object::{builtin, environment::Environment, object::Object},
    parser::{
        expression::Expression,
        infix::Infix,
        prefix::Prefix,
        statement::{BlockStatement, Program, Statement},
    },
};

use super::eval_error::EvalErr;

pub fn eval(node: &Program, env: Rc<RefCell<Environment>>) -> Result<Object, EvalErr> {
    let mut res = Object::Null;
    for statement in &node.statements {
        res = eval_statement(statement, Rc::clone(&env))?;
        if let Object::Return(value) = res {
            return Ok(*value);
        }
    }
    Ok(res)
}

fn eval_statement(statement: &Statement, env: Rc<RefCell<Environment>>) -> Result<Object, EvalErr> {
    match statement {
        Statement::Let(name, exp) => {
            let mut result = eval_expression(exp, Rc::clone(&env))?;
            if let Object::Return(value) = result {
                result = *value;
            }
            env.borrow_mut().set(name, result.clone());
            Ok(result)
        }
        Statement::Return(Some(exp)) => {
            let result = eval_expression(exp, env)?;
            Ok(Object::Return(Box::new(result)))
        }
        Statement::Return(None) => Ok(Object::Return(Box::new(Object::Null))),
        Statement::Expression(expr) => eval_expression(expr, env),
    }
}

fn eval_expression(
    expression: &Expression,
    env: Rc<RefCell<Environment>>,
) -> Result<Object, EvalErr> {
    match &expression {
        Expression::Infix(op, l, r) => eval_infix_expression(op, l, r, env),
        Expression::Prefix(operator, expressions) => {
            eval_prefix_expressions(operator, expressions, env)
        }
        Expression::Identifier(name) => eval_identifier(name, env),
        Expression::IntegerLiteral(value) => Ok(Object::Integer(*value)),
        Expression::FloatLiteral(value) => Ok(Object::Float(*value)),
        Expression::BooleanLiteral(value) => Ok(Object::Boolean(*value)),
        Expression::If(condition, consequence, alternative) => {
            eval_if_expression(condition, consequence, alternative, env)
        }
        Expression::Empty => Ok(Object::Null),
        Expression::FunctionLiteral(params, body) => {
            Ok(Object::Function(params.to_vec(), body.clone(), env))
        }
        Expression::Call(func, args) => {
            let function = eval_expression(func, Rc::clone(&env))?;
            let arguments = eval_expressions(args, env)?;
            apply_function(function, arguments)
        }
        Expression::StringLiteral(s) => Ok(Object::String(s.to_string())),
        Expression::ForLoop(condition, consequence) => {
            eval_forloop_expression(condition, consequence, env)
        }
        Expression::Assign(ident, op, expr) => eval_assign_expression(ident, op, expr, env),
    }
}

fn eval_forloop_expression(
    condition: &Expression,
    consequence: &BlockStatement,
    env: Rc<RefCell<Environment>>,
) -> Result<Object, EvalErr> {
    let mut rt = Object::Boolean(true);
    loop {
        let condition = eval_expression(condition, Rc::clone(&env))?;
        if condition == Object::Boolean(true) {
            rt = eval_block_statement(consequence, Rc::clone(&env))?;
            if let Object::Return(return_val) = rt {
                return Ok(*return_val);
            }
        } else {
            break;
        }
    }
    Ok(rt)
}

fn eval_expressions(
    exps: &[Expression],
    env: Rc<RefCell<Environment>>,
) -> Result<Vec<Object>, EvalErr> {
    let mut results = vec![];
    for exp in exps {
        results.push(eval_expression(exp, Rc::clone(&env))?);
    }
    Ok(results)
}

fn apply_function(function: Object, arguments: Vec<Object>) -> Result<Object, EvalErr> {
    match function {
        Object::Function(params, body, env) => {
            if arguments.len() != params.len() {
                return Err(EvalErr::WrongArgumentCount(params.len(), arguments.len()));
            };
            let new_env = extend_function_env(params, arguments, env);
            let evaluated = eval_block_statement(&body, new_env)?;
            match evaluated {
                Object::Return(value) => Ok(*value),
                _ => Ok(evaluated),
            }
        }
        Object::Builtin(func) => func(arguments),
        _ => Err(EvalErr::NotCallable(function.clone())),
    }
}

fn extend_function_env(
    params: Vec<String>,
    arguments: Vec<Object>,
    env: Rc<RefCell<Environment>>,
) -> Rc<RefCell<Environment>> {
    let new_env = Rc::new(RefCell::new(Environment::extend(env)));
    for (i, param) in params.iter().enumerate() {
        let arg = (arguments.get(i)).cloned().unwrap_or(Object::Null);
        new_env.borrow_mut().set(param, arg);
    }
    new_env
}

fn eval_identifier(name: &str, env: Rc<RefCell<Environment>>) -> Result<Object, EvalErr> {
    if let Some(obj) = env.borrow().get(name) {
        return Ok(obj);
    }
    if let Some(obj) = builtin::lookup(name) {
        return Ok(obj);
    }
    Err(EvalErr::IdentifierNotFound(name.to_string()))
}

fn eval_if_expression(
    condition: &Expression,
    consequence: &BlockStatement,
    alternative: &Option<BlockStatement>,
    env: Rc<RefCell<Environment>>,
) -> Result<Object, EvalErr> {
    let result = eval_expression(condition, Rc::clone(&env))?;
    if result.is_truthy()? {
        eval_block_statement(consequence, env)
    } else {
        alternative
            .clone()
            .map(|block| eval_block_statement(&block, env))
            .unwrap_or(Ok(Object::Null))
    }
}

fn eval_block_statement(
    block: &BlockStatement,
    env: Rc<RefCell<Environment>>,
) -> Result<Object, EvalErr> {
    for statement in &block.statements {
        let res = eval_statement(statement, Rc::clone(&env))?;
        if let Object::Return(_) = res {
            return Ok(res);
        }
    }
    Ok(Object::Null)
}

fn eval_prefix_expressions(
    prefix: &Prefix,
    expression: &Expression,
    env: Rc<RefCell<Environment>>,
) -> Result<Object, EvalErr> {
    let object = eval_expression(expression, env)?;
    match prefix {
        Prefix::Bang => Ok(Object::Boolean(!object.is_truthy()?)),
        Prefix::Minus => match object {
            Object::Integer(val) => Ok(Object::Integer(-val)),
            _ => Err(EvalErr::CannotApplyPrefix(Prefix::Minus, object)),
        },
    }
}

fn eval_infix_expression(
    infix: &Infix,
    left: &Expression,
    right: &Expression,
    env: Rc<RefCell<Environment>>,
) -> Result<Object, EvalErr> {
    let l_obj = eval_expression(left, Rc::clone(&env))?;
    let r_obj = eval_expression(right, Rc::clone(&env))?;

    Ok(eval_infix_object(infix, &l_obj, &r_obj)?)
}

fn eval_infix_object(infix: &Infix, left: &Object, right: &Object) -> Result<Object, EvalErr> {
    match (left, right) {
        (Object::Boolean(l_o), Object::Boolean(r_o)) => {
            eval_boolean_infix_expression(infix, *l_o, *r_o)
        }
        (Object::Integer(l_o), Object::Integer(r_o)) => {
            eval_integer_infix_expression(infix, *l_o, *r_o)
        }
        (Object::Integer(l_o), Object::Float(r_o)) => {
            eval_float_infix_expression(infix, *l_o as f64, *r_o)
        }
        (Object::Float(l_o), Object::Integer(r_o)) => {
            eval_float_infix_expression(infix, *l_o, *r_o as f64)
        }
        (Object::Float(l_o), Object::Float(r_o)) => eval_float_infix_expression(infix, *l_o, *r_o),
        (Object::String(l_o), Object::String(r_o)) => {
            eval_string_infix_expression(infix, &l_o, &r_o)
        }
        (l_o, r_o) => Err(EvalErr::IncompatibleTypes(
            infix.clone(),
            l_o.clone(),
            r_o.clone(),
        )),
    }
}

fn eval_integer_infix_expression(
    infix: &Infix,
    left: i128,
    right: i128,
) -> Result<Object, EvalErr> {
    match infix {
        Infix::Eq => Ok(Object::Boolean(left == right)),
        Infix::NotEq => Ok(Object::Boolean(left != right)),
        Infix::Lt => Ok(Object::Boolean(left < right)),
        Infix::Gt => Ok(Object::Boolean(left > right)),
        Infix::Plus => Ok(Object::Integer(left + right)),
        Infix::Minus => Ok(Object::Integer(left - right)),
        Infix::Asterisk => Ok(Object::Integer(left * right)),
        Infix::Slash => Ok(Object::Integer(left / right)),
        Infix::PlusEquals => Ok(Object::Integer(left + right)),
        Infix::MinusEquals => Ok(Object::Integer(left - right)),
        Infix::SlashEuqals => Ok(Object::Integer(left / right)),
        Infix::AsteriskEquals => Ok(Object::Integer(left * right)),
        _ => Err(EvalErr::UnknownInfixOperator(
            infix.clone(),
            Object::Integer(left),
            Object::Integer(right),
        )),
    }
}

fn eval_float_infix_expression(infix: &Infix, left: f64, right: f64) -> Result<Object, EvalErr> {
    match infix {
        Infix::Eq => Ok(Object::Boolean(left == right)),
        Infix::NotEq => Ok(Object::Boolean(left != right)),
        Infix::Lt => Ok(Object::Boolean(left < right)),
        Infix::Gt => Ok(Object::Boolean(left > right)),
        Infix::Plus => Ok(Object::Float(left + right)),
        Infix::Minus => Ok(Object::Float(left - right)),
        Infix::Asterisk => Ok(Object::Float(left * right)),
        Infix::Slash => Ok(Object::Float(left / right)),
        Infix::PlusEquals => Ok(Object::Float(left + right)),
        Infix::MinusEquals => Ok(Object::Float(left - right)),
        Infix::SlashEuqals => Ok(Object::Float(left / right)),
        Infix::AsteriskEquals => Ok(Object::Float(left * right)),
        _ => Err(EvalErr::UnknownInfixOperator(
            infix.clone(),
            Object::Float(left),
            Object::Float(right),
        )),
    }
}

fn eval_string_infix_expression(infix: &Infix, left: &str, right: &str) -> Result<Object, EvalErr> {
    match infix {
        Infix::Plus => Ok(Object::String([left, right].concat())),
        _ => Err(EvalErr::UnknownInfixOperator(
            infix.clone(),
            Object::String(left.to_string()),
            Object::String(right.to_string()),
        )),
    }
}

fn eval_boolean_infix_expression(
    infix: &Infix,
    left: bool,
    right: bool,
) -> Result<Object, EvalErr> {
    match infix {
        Infix::Eq => Ok(Object::Boolean(left == right)),
        Infix::NotEq => Ok(Object::Boolean(left != right)),
        _ => Err(EvalErr::UnsupportedOperand(
            infix.clone(),
            Object::Boolean(left),
            Object::Boolean(right),
        )),
    }
}

fn eval_assign_expression(
    ident: &str,
    op: &Infix,
    expr: &Expression,
    env: Rc<RefCell<Environment>>,
) -> Result<Object, EvalErr> {
    let current = eval_identifier(ident, Rc::clone(&env))?;
    let evaluated = eval_expression(expr, Rc::clone(&env))?;

    match op {
        Infix::Assign => {
            env.borrow_mut().set(ident, evaluated.clone());
        }
        Infix::PlusEquals => {
            let res = eval_infix_object(&Infix::PlusEquals, &current, &evaluated)?;
            env.borrow_mut().set(ident, res);
        }
        Infix::MinusEquals => {
            let res = eval_infix_object(&Infix::MinusEquals, &current, &evaluated)?;
            env.borrow_mut().set(ident, res);
        }
        Infix::AsteriskEquals => {
            let res = eval_infix_object(&Infix::AsteriskEquals, &current, &evaluated)?;
            env.borrow_mut().set(ident, res);
        }
        Infix::SlashEuqals => {
            let res = eval_infix_object(&Infix::SlashEuqals, &current, &evaluated)?;
            env.borrow_mut().set(ident, res);
        }
        _ => {
            return Err(EvalErr::UnknownInfixOperator(
                op.clone(),
                current,
                evaluated,
            ))
        }
    };
    Ok(evaluated)
}
