#[cfg(test)]
pub mod tests {
    use std::{cell::RefCell, rc::Rc};

    use crate::parser::expression::Expression;
    use crate::parser::statement::{BlockStatement, Statement};
    use crate::{
        evaluator::{eval_error::EvalErr, evaluator::eval},
        lexer::lexer::Lexer,
        object::{environment::Environment, object::Object},
        parser::{infix::Infix, parser::Parser, prefix::Prefix},
    };

    fn eval_input(input: &str) -> Result<Object, EvalErr> {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let parse_res = parser.parse_program();
        let env = Rc::new(RefCell::new(Environment::new()));
        match parse_res {
            Ok(prog) => return eval(&prog, env),
            Err(err) => panic!("Error during parsing: \n{:?} For input: \n{}\n", err, input),
        }
    }

    fn assert_input_against_object(input_expected: Vec<(&str, Object)>) {
        for (input, expected) in input_expected {
            let eval = eval_input(input);
            match eval {
                Ok(obj) => {
                    //let to_compare = match obj {
                    //  Object::Return(ret) => *ret,
                    //_ => obj,
                    //};
                    assert_eq!(obj, expected, "\n\tfor input: {}\n", input);
                }
                Err(err) => {
                    panic!("{:?} for input: {}", err, input);
                }
            }
        }
    }

    fn assert_input_against_expected_error(input_expected: Vec<(&str, EvalErr)>) {
        for (input, expected_err) in input_expected {
            let eval = eval_input(input);
            match eval {
                Ok(obj) => {
                    panic!("no error, got Object {:?}", obj);
                }
                Err(err) => {
                    assert_eq!(err, expected_err, "\n\tfor input: {}\n", input);
                }
            }
        }
    }

    #[test]
    fn test_integer_expression() {
        let input = vec![
            ("5", Object::Integer(5)),
            ("10", Object::Integer(10)),
            ("1", Object::Integer(1)),
            ("0", Object::Integer(0)),
            ("999999999999999", Object::Integer(999999999999999)),
            ("-5", Object::Integer(-5)),
            ("-10", Object::Integer(-10)),
        ];
        assert_input_against_object(input);
    }

    #[test]
    fn test_boolean_expression() {
        let input = vec![
            ("true", Object::Boolean(true)),
            ("false", Object::Boolean(false)),
        ];
        assert_input_against_object(input);
    }

    #[test]
    fn test_prefix_operator() {
        let input = vec![
            ("!true", Object::Boolean(false)),
            ("!false", Object::Boolean(true)),
            ("!!true", Object::Boolean(true)),
            ("!!false", Object::Boolean(false)),
        ];
        assert_input_against_object(input);
    }

    #[test]
    fn test_integer_infix_calc_expression() {
        let input = vec![
            ("1+1", Object::Integer(2)),
            ("5+5+5+5-10", Object::Integer(10)),
            ("10*2*4", Object::Integer(80)),
            ("-50+100+-50", Object::Integer(0)),
            ("5*2+10", Object::Integer(20)),
            ("5+2*10", Object::Integer(25)),
            ("20+2*-10", Object::Integer(0)),
            ("50/2*2+10", Object::Integer(60)),
            ("2*(5+10)", Object::Integer(30)),
            ("3*3*3+10", Object::Integer(37)),
            ("3*(3*3)+10", Object::Integer(37)),
            ("(5+10*2+15/3)*2+-10", Object::Integer(50)),
        ];
        assert_input_against_object(input);
    }

    #[test]
    fn test_integer_infix_cmp_expression() {
        let input = vec![
            ("1==1", Object::Boolean(true)),
            ("1!=1", Object::Boolean(false)),
            ("1>1", Object::Boolean(false)),
            ("1<1", Object::Boolean(false)),
            ("1==10", Object::Boolean(false)),
            ("1!=112", Object::Boolean(true)),
            ("5>2", Object::Boolean(true)),
            ("90<2342", Object::Boolean(true)),
            ("-43>320", Object::Boolean(false)),
            ("-43<5934", Object::Boolean(true)),
        ];
        assert_input_against_object(input);
    }

    #[test]
    fn test_boolean_infix_cmp_expression() {
        let input = vec![
            ("true==true", Object::Boolean(true)),
            ("true==false", Object::Boolean(false)),
            ("false==false", Object::Boolean(true)),
            ("false==true", Object::Boolean(false)),
            ("true!=true", Object::Boolean(false)),
            ("true!=false", Object::Boolean(true)),
            ("false!=false", Object::Boolean(false)),
            ("false!=true", Object::Boolean(true)),
            ("(1<2)==true", Object::Boolean(true)),
            ("(1<2)==false", Object::Boolean(false)),
            ("(1>2)==true", Object::Boolean(false)),
            ("(1>2)!=true", Object::Boolean(true)),
        ];
        assert_input_against_object(input);
    }

    #[test]
    fn test_if_else_expression() {
        let input = vec![
            ("if(true){ return 10; };", Object::Integer(10)),
            ("if(false){ return 10; };", Object::Null),
            (
                "if(false){ return 10; } else { return 20; };",
                Object::Integer(20),
            ),
            (
                "if(true){ return 0; } else {return 20;};",
                Object::Integer(0),
            ),
        ];
        assert_input_against_object(input);
    }

    #[test]
    fn test_return_statement() {
        let input = vec![
            ("return 10;", Object::Integer(10)),
            ("return 10; 9;", Object::Integer(10)),
            ("return 2*5; 9", Object::Integer(10)),
            ("9; return 2*5; 9", Object::Integer(10)),
            (
                "if(true) 
                { 
                    if(true) 
                    {
                        return 10; 
                    };
                    return 0; 
                };
                ",
                Object::Integer(10),
            ),
        ];
        assert_input_against_object(input);
    }

    #[test]
    fn test_let_statement() {
        let input = vec![
            ("let a = 5; a", Object::Integer(5)),
            ("let a = 5 * 5; a", Object::Integer(25)),
            ("let a = 5; let b = a; b", Object::Integer(5)),
            (
                "let a = 5; let b = a; let c = a + b + 5; c",
                Object::Integer(15),
            ),
        ];
        assert_input_against_object(input);
    }

    #[test]
    fn test_function() {
        let input = vec![(
            "function(x){ x + 2; }",
            Object::Function(
                vec!["x".to_string()],
                BlockStatement {
                    statements: [Statement::Expression(Expression::Infix(
                        Infix::Plus,
                        Box::new(Expression::Identifier("x".to_string())),
                        Box::new(Expression::IntegerLiteral(2)),
                    ))]
                    .to_vec(),
                },
                Rc::new(RefCell::new(Environment::new())),
            ),
        )];
        assert_input_against_object(input);
    }

    #[test]
    fn test_errors() {
        let input = vec![
            (
                "-true;",
                EvalErr::CannotApplyPrefix(Prefix::Minus, Object::Boolean(true)),
            ),
            (
                "false * true;",
                EvalErr::UnsupportedOperand(
                    Infix::Asterisk,
                    Object::Boolean(false),
                    Object::Boolean(true),
                ),
            ),
            (
                "5 + true;",
                EvalErr::IncompatibleTypes(Infix::Plus, Object::Integer(5), Object::Boolean(true)),
            ),
            (
                "!5;",
                EvalErr::CannotApplyPrefix(Prefix::Bang, Object::Integer(5)),
            ),
            ("foobar", EvalErr::IdentifierNotFound("foobar".to_string())),
        ];
        assert_input_against_expected_error(input);
    }
}
