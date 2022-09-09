use std::fmt::Debug;

use crate::{
    ast::{BlockStatement, Expression, Infix, Prefix, Program, Statement},
    lexer::Lexer,
    parse_error::ParseErr,
    token::Token,
};

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<ParseErr>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            current_token: Token::Illegal,
            peek_token: Token::Illegal,
            errors: vec![],
        };
        parser.next_token();
        parser.next_token();
        return parser;
    }

    pub fn input(&self) -> &str {
        &self.lexer.input
    }

    pub fn errors(&self) -> &[ParseErr] {
        &self.errors
    }

    pub fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn expect_peek(
        &mut self,
        token: Token,
        expected: fn(Token) -> ParseErr,
    ) -> Result<(), ParseErr> {
        if self.peek_token != token {
            return Err(expected(self.peek_token.clone()));
        }
        self.next_token();
        Ok(())
    }

    pub fn parse_program(&mut self) -> Program {
        let mut statements = vec![];
        while self.current_token != Token::Eof {
            let statement = self.parse_statement();
            match statement {
                Ok(stm) => statements.push(stm),
                Err(err) => self.errors.push(err),
            };
            self.next_token();
        }
        return Program { statements };
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseErr> {
        match self.current_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn get_prefix_fn(&self) -> Option<PrefixParseFn> {
        match &self.current_token {
            Token::Identifier(_) => Some(Parser::parse_identifier_expression),
            Token::Int(_) => Some(Parser::parse_integer_expression),
            Token::Bang => Some(Parser::parse_prefix_expression),
            Token::Minus => Some(Parser::parse_prefix_expression),
            Token::True | Token::False => Some(Parser::parse_boolean_expression),
            Token::LParenthesis => Some(Parser::parse_grouped_expression),
            Token::If => Some(Parser::parse_if_expression),
            Token::Function => Some(Parser::parse_function_expression),
            _ => None,
        }
    }

    fn get_infix_fn(&mut self) -> Option<InfixParseFn> {
        match &self.peek_token {
            Token::Plus
            | Token::Minus
            | Token::Asterisk
            | Token::Slash
            | Token::Equal
            | Token::NotEqual
            | Token::Lt
            | Token::Gt => Some(Parser::parse_infix_expression),
            Token::LParenthesis => Some(Parser::parse_call_expression),
            _ => None,
        }
    }

    fn get_prefix_token(&self, token: &Token) -> Result<Prefix, ParseErr> {
        match token {
            Token::Bang => Ok(Prefix::Bang),
            Token::Minus => Ok(Prefix::Minus),
            token => Err(ParseErr::ExpectedPrefixToken(token.clone())),
        }
    }

    fn get_infix_token(&self, token: &Token) -> (Precedence, Option<Infix>) {
        match token {
            Token::Equal => (Precedence::Equals, Some(Infix::Eq)),
            Token::NotEqual => (Precedence::Equals, Some(Infix::NotEq)),
            Token::Lt => (Precedence::LessGreater, Some(Infix::Lt)),
            Token::Gt => (Precedence::LessGreater, Some(Infix::Gt)),
            Token::Plus => (Precedence::Sum, Some(Infix::Plus)),
            Token::Minus => (Precedence::Sum, Some(Infix::Minus)),
            Token::Slash => (Precedence::Product, Some(Infix::Slash)),
            Token::Asterisk => (Precedence::Product, Some(Infix::Asterisk)),
            Token::LParenthesis => (Precedence::Call, None),
            _ => (Precedence::Lowest, None),
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParseErr> {
        let prefix = self
            .get_prefix_fn()
            .ok_or_else(|| ParseErr::ExpectedPrefixToken(self.current_token.clone()))?;
        let mut left_expr = prefix(self)?;

        while self.peek_token != Token::Semicolon
            && precedence < self.get_infix_token(&self.peek_token).0
        {
            if let Some(infix) = self.get_infix_fn() {
                self.next_token();
                left_expr = infix(self, left_expr)?;
            } else {
                return Ok(left_expr);
            }
        }
        Ok(left_expr)
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParseErr> {
        self.next_token();
        let name = self.parse_identifier_string()?;
        self.expect_peek(Token::Assign, ParseErr::ExpectedAssign)?;
        self.next_token();
        let value = self.parse_expression(Precedence::Lowest)?;
        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Ok(Statement::Let(name, value))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParseErr> {
        self.next_token();
        if self.current_token == Token::Semicolon {
            return Ok(Statement::Return(None));
        }
        let expression = self.parse_expression(Precedence::Lowest)?;
        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Ok(Statement::Return(Some(expression)))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParseErr> {
        let expression = self.parse_expression(Precedence::Lowest);
        if self.peek_token == Token::Semicolon {
            self.next_token();
        }
        expression.map(Statement::Expression)
    }

    fn parse_identifier_expression(&mut self) -> Result<Expression, ParseErr> {
        self.parse_identifier_string().map(Expression::Identifier)
    }

    fn parse_identifier_string(&self) -> Result<String, ParseErr> {
        if let Token::Identifier(ident) = &self.current_token {
            Ok(ident.to_string())
        } else {
            Err(ParseErr::ExpectedIdentifierToken(
                self.current_token.clone(),
            ))
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, ParseErr> {
        let prefix_token = self.get_prefix_token(&self.current_token)?;
        self.next_token();
        let right_expr = self.parse_expression(Precedence::Prefix)?;
        Ok(Expression::Prefix(prefix_token, Box::new(right_expr)))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, ParseErr> {
        let (precedence, infix) = self.get_infix_token(&self.current_token);
        let i = infix.ok_or_else(|| ParseErr::ExpectedInfixToken(self.current_token.clone()))?;
        self.next_token();
        let right = self.parse_expression(precedence)?;

        Ok(Expression::Infix(i, Box::new(left), Box::new(right)))
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, ParseErr> {
        self.next_token();
        let expr = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(Token::RParenthesis, ParseErr::ExpectedRparen)?;
        return Ok(expr);
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement, ParseErr> {
        let mut statements = vec![];

        self.next_token();
        while self.current_token != Token::RBrace && self.current_token != Token::Eof {
            statements.push(self.parse_statement()?);
            self.next_token();
        }

        Ok(BlockStatement { statements })
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ParseErr> {
        self.expect_peek(Token::LParenthesis, ParseErr::ExpectedLparen)?;
        self.next_token();
        let condition = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(Token::RParenthesis, ParseErr::ExpectedRparen)?;
        self.expect_peek(Token::LBrace, ParseErr::ExpectedLbrace)?;
        let consequence = self.parse_block_statement()?;
        let alternative = if self.peek_token == Token::Else {
            self.next_token();
            self.expect_peek(Token::LBrace, ParseErr::ExpectedLbrace)?;
            Some(self.parse_block_statement()?)
        } else {
            None
        };
        Ok(Expression::If(
            Box::new(condition),
            consequence,
            alternative,
        ))
    }

    fn parse_function_expression(&mut self) -> Result<Expression, ParseErr> {
        self.expect_peek(Token::LParenthesis, ParseErr::ExpectedLparen)?;
        let parameters = self.parse_function_parameters()?;
        self.expect_peek(Token::LBrace, ParseErr::ExpectedLbrace)?;
        let body = self.parse_block_statement()?;
        Ok(Expression::FunctionLiteral(parameters, body))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<String>, ParseErr> {
        let mut identifiers = vec![];
        if self.peek_token == Token::RParenthesis {
            self.next_token();
            return Ok(identifiers);
        }
        self.next_token();
        identifiers.push(self.parse_identifier_string()?);

        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();
            identifiers.push(self.parse_identifier_string()?);
        }
        self.expect_peek(Token::RParenthesis, ParseErr::ExpectedRparen)?;

        Ok(identifiers)
    }

    fn parse_call_expression(&mut self, function: Expression) -> Result<Expression, ParseErr> {
        let arguments = self.parse_expressions(Token::RParenthesis, ParseErr::ExpectedRparen)?;
        Ok(Expression::Call(Box::new(function), arguments))
    }

    fn parse_expressions(
        &mut self,
        closing_token: Token,
        expected: fn(Token) -> ParseErr,
    ) -> Result<Vec<Expression>, ParseErr> {
        let mut exps = vec![];
        if self.peek_token == closing_token {
            self.next_token();
            return Ok(exps);
        }
        self.next_token();
        exps.push(self.parse_expression(Precedence::Lowest)?);
        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();
            exps.push(self.parse_expression(Precedence::Lowest)?);
        }
        self.expect_peek(closing_token, expected)?;

        Ok(exps)
    }

    fn parse_integer_expression(&mut self) -> Result<Expression, ParseErr> {
        if let Token::Int(int) = &self.current_token {
            match int.parse() {
                Ok(value) => Ok(Expression::IntegerLiteral(value)),
                Err(_) => Err(ParseErr::ParseInt(int.to_string())),
            }
        } else {
            Err(ParseErr::ExpectedIntegerToken(self.current_token.clone()))
        }
    }

    fn parse_boolean_expression(&mut self) -> Result<Expression, ParseErr> {
        match &self.current_token {
            Token::True => Ok(Expression::BooleanLiteral(true)),
            Token::False => Ok(Expression::BooleanLiteral(false)),
            _ => Err(ParseErr::ExpectedBoolToken(self.current_token.clone())),
        }
    }
}

type PrefixParseFn = fn(&mut Parser) -> Result<Expression, ParseErr>;
type InfixParseFn = fn(&mut Parser, Expression) -> Result<Expression, ParseErr>;

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

#[cfg(test)]
pub mod parser_test {
    use crate::ast::{Expression, Infix, Statement};
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn check_parser_errors(parser: &Parser) {
        let errors = &parser.errors;
        if errors.len() > 0 {
            panic!(
                "\nInput:\n'{}' \nGot Parser Errors:\n- {}\n",
                parser.lexer.input,
                errors
                    .iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<String>>()
                    .join(", \n- ")
            );
        }
    }

    fn check_str_str_eq(intput_output: Vec<(&str, &str)>) {
        for (input, expected) in intput_output {
            let lexer = Lexer::new(input.to_owned());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_parser_errors(&parser);
            assert_eq!(program.to_string(), expected);
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

        check_parser_errors(&parser);
        assert_eq!(expected, program.statements,);
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

        check_parser_errors(&parser);
        assert_eq!(expected, program.statements,);
    }

    #[test]
    fn test_integer_expression() {
        let input = "5";
        let lexer = Lexer::new(input.to_owned());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let expected = vec![Statement::Expression(Expression::IntegerLiteral(5))];

        check_parser_errors(&parser);
        assert_eq!(expected, program.statements);
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
            check_parser_errors(&parser);

            assert_eq!(
                program.statements,
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
            check_parser_errors(&parser);

            assert_eq!(
                program.statements,
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
