#[cfg(test)]
pub mod tests {

    use crate::{
        lexer::lexer::Lexer,
        parser::{expression::Expression, infix::Infix, parser::Parser, statement::Statement},
    };

    fn check_str_str_eq(intput_output: Vec<(&str, &str)>) {
        for (input, expected) in intput_output {
            let lexer = Lexer::new(input.to_owned());
            let mut parser = Parser::new(lexer);
            let parse_res = parser.parse_program();

            match parse_res {
                Ok(program) => assert_eq!(program.to_string(), expected),
                Err(err) => panic!("Got error: {:?}", err),
            }
        }
    }

    #[test]
    fn test_let_stmt() {
        let input = "
        let x = 5;
        let y = true;
        let foobar = y;
        ";
        let lexer = Lexer::new(input.to_owned());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        let expected = vec![
            Statement::Let("x".to_string(), Expression::IntegerLiteral(5)),
            Statement::Let("y".to_string(), Expression::BooleanLiteral(true)),
            Statement::Let(
                "foobar".to_string(),
                Expression::Identifier("y".to_string()),
            ),
        ];

        assert_eq!(expected, program.unwrap().statements);
    }

    #[test]
    fn test_return_stmt() {
        let input = "
        return;
        return foobar;
        return true;
        ";
        let lexer = Lexer::new(input.to_owned());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        let expected = vec![
            Statement::Return(None),
            Statement::Return(Some(Expression::Identifier("foobar".to_string()))),
            Statement::Return(Some(Expression::BooleanLiteral(true))),
        ];

        assert_eq!(expected, program.unwrap().statements);
    }

    #[test]
    fn test_integer_expression() {
        let input = "5";
        let lexer = Lexer::new(input.to_owned());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let expected = vec![Statement::Expression(Expression::IntegerLiteral(5))];

        assert_eq!(expected, program.unwrap().statements);
    }

    #[test]
    fn test_infix_expression() {
        let tests = vec![
            ("5 + 5;", 5, Infix::Plus, 5),
            ("5 - 5;", 5, Infix::Minus, 5),
            ("5 * 5;", 5, Infix::Asterisk, 5),
            ("5 / 5;", 5, Infix::Slash, 5),
            ("5 > 5;", 5, Infix::Gt, 5),
            ("5 < 5;", 5, Infix::Lt, 5),
            ("5 == 5;", 5, Infix::Eq, 5),
            ("5 != 5;", 5, Infix::NotEq, 5),
        ];
        for (input, left, operator, right) in tests {
            let lexer = Lexer::new(input.to_owned());
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();

            assert_eq!(
                program.unwrap().statements,
                vec![Statement::Expression(Expression::Infix(
                    operator,
                    Box::new(Expression::IntegerLiteral(left)),
                    Box::new(Expression::IntegerLiteral(right))
                ))]
            );
        }
    }

    #[test]
    fn test_identifier() {
        let intput_output = vec![("foobar;", "foobar;"), ("return ident;", "return ident;")];

        check_str_str_eq(intput_output);
    }

    #[test]
    fn test_boolean() {
        let intput_output = vec![
            ("true;", "true;"),
            ("false;", "false;"),
            ("return true;", "return true;"),
            ("return false;", "return false;"),
            ("12 > 5 == true", "((12 > 5) == true);"),
            ("12 < 5 != !false", "((12 < 5) != (!false));"),
        ];

        check_str_str_eq(intput_output);
    }

    #[test]
    fn infix_expression_boolean() {
        let tests = vec![
            ("true == true", true, Infix::Eq, true),
            ("true != false", true, Infix::NotEq, false),
            ("false == false", false, Infix::Eq, false),
        ];
        for (input, left, operator, right) in tests {
            let lexer = Lexer::new(input.to_owned());
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();

            assert_eq!(
                program.unwrap().statements,
                vec![Statement::Expression(Expression::Infix(
                    operator,
                    Box::new(Expression::BooleanLiteral(left)),
                    Box::new(Expression::BooleanLiteral(right))
                ))]
            );
        }
    }

    #[test]
    fn test_function_literal() {
        let intput_output = vec![
            ("function(x, y) { x + y; }", "function(x, y) { (x + y); };"),
            ("function() { 3 * 9; }", "function() { (3 * 9); };"),
            ("function(x) { x * 9; }", "function(x) { (x * 9); };"),
            ("function(x, y) { x + y; }", "function(x, y) { (x + y); };"),
        ];
        check_str_str_eq(intput_output);
    }

    #[test]
    fn test_function_call() {
        let intput_output = vec![
            ("call()", "call();"),
            ("add(1, 2 * 3, 4 + 5)", "add(1, (2 * 3), (4 + 5));"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d);"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)));",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g));",
            ),
            (
                "function(x, y) { x + y; }(3, 4)",
                "function(x, y) { (x + y); }(3, 4);",
            ),
        ];
        check_str_str_eq(intput_output);
    }

    #[test]
    fn test_operator_precedence() {
        let intput_output = vec![
            ("-a * b", "((-a) * b);"),
            ("!-a", "(!(-a));"),
            ("a + b + c", "((a + b) + c);"),
            ("a + b - c", "((a + b) - c);"),
            ("a * b * c", "((a * b) * c);"),
            ("a * b / c", "((a * b) / c);"),
            ("a + b / c", "(a + (b / c));"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f);"),
            ("3 + 4; -5 * 5", "(3 + 4);((-5) * 5);"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4));"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4));"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));",
            ),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4);"),
            ("(5 + 5) * 2", "((5 + 5) * 2);"),
            ("2 / (5 + 5)", "(2 / (5 + 5));"),
            ("-(5 + 5)", "(-(5 + 5));"),
            ("!(true == true)", "(!(true == true));"),
            ("if (x < y) { x }", "if (x < y) { x; };"),
            (
                "if (x < y) { x } else { y }",
                "if (x < y) { x; } else { y; };",
            ),
            (
                "function(x, y) { x + y; }(3, 4)",
                "function(x, y) { (x + y); }(3, 4);",
            ),
        ];

        check_str_str_eq(intput_output);
    }
}
