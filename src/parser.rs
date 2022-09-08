use crate::{
    ast::{Expression, Infix, Prefix, Program, Statement},
    lexer::Lexer,
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

    fn get_prefix_fn(&self) -> Option<PrefixParseFn> {
        match &self.current_token {
            Token::Identifier(_) => Some(Parser::parse_identifier_expression),
            Token::Int(_) => Some(Parser::parse_integer_expression),
            Token::Bang => Some(Parser::parse_prefix_expression),
            Token::Minus => Some(Parser::parse_prefix_expression),
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

#[derive(Debug)]
pub enum ParseErr {
    ExpectedPrefixToken(Token),
    ExpectedInfixToken(Token),
    ExpectedIdentifierToken(Token),
    ExpectedBooleanToken(Token),
    ExpectedIntegerToken(Token),
    ExpectedFloatToken(Token),
    ExpectedStringToken(Token),
    ExpectedLparen(Token),
    ExpectedRparen(Token),
    ExpectedLbrace(Token),
    ExpectedRbrace(Token),
    ExpectedRbracket(Token),
    ExpectedAssign(Token),
    ExpectedSemicolon(Token),
    ExpectedComma(Token),
    ExpectedColon(Token),
    ParseInt(String),
    ParseFloat(String),
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
                "for input '{}', got parser errors: {:?}",
                parser.lexer.input, errors
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
        let y = 1023;
        let foobar = 43243243;
        ";
        let lexer = Lexer::new(input.to_owned());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        let expected = vec![
            Statement::Let("x".to_string(), Expression::IntegerLiteral(5)),
            Statement::Let("y".to_string(), Expression::IntegerLiteral(1023)),
            Statement::Let("foobar".to_string(), Expression::IntegerLiteral(43243243)),
        ];

        check_parser_errors(&parser);
        assert_eq!(expected, program.statements,);
    }

    #[test]
    fn test_return_stmt() {
        let input = "
        return;
        return 5;
        return 1043;
        ";
        let lexer = Lexer::new(input.to_owned());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        let expected = vec![
            Statement::Return(None),
            Statement::Return(Some(Expression::IntegerLiteral(5))),
            Statement::Return(Some(Expression::IntegerLiteral(1043))),
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
    fn operator_precedence() {
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
        ];

        check_str_str_eq(intput_output);
    }
}
