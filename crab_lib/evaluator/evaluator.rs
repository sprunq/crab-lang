use std::{cell::RefCell, rc::Rc};

use crate::{
    object::{environment::Environment, object::Object},
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
            // TODO: Is this `clone()` the right way to do?
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
        Expression::FloatLiteral(_) => todo!(),
        Expression::BooleanLiteral(value) => Ok(Object::Boolean(*value)),
        Expression::If(condition, consequence, alternative) => {
            eval_if_expression(condition, consequence, alternative, env)
        }
        Expression::Empty => todo!(),
        Expression::FunctionLiteral(_, _) => todo!(),
        Expression::Call(_, _) => todo!(),
    }
}

fn eval_identifier(name: &str, env: Rc<RefCell<Environment>>) -> Result<Object, EvalErr> {
    if let Some(obj) = env.borrow().get(name) {
        return Ok(obj.clone());
    }
    /*
    if let Some(obj) = builtin::lookup(name) {
        return Ok(obj);
    }
    */
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
    // let mut res = Object::Null;
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
    let r_obj = eval_expression(right, env)?;

    match (l_obj, r_obj) {
        (Object::Boolean(left), Object::Boolean(right)) => {
            eval_boolean_infix_expression(infix, left, right)
        }
        (Object::Integer(left), Object::Integer(right)) => {
            eval_integer_infix_expression(infix, left, right)
        }
        (left, right) => Err(EvalErr::IncompatibleTypes(infix.clone(), left, right)),
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
