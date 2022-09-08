use crate::{
    ast::{Expression, Program, Statement},
    lexer::Lexer,
    token::Token,
};

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<ParserError>,
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

    pub fn errors(&self) -> &[ParserError] {
        &self.errors
    }

    pub fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn expect_peek(
        &mut self,
        token: Token,
        expected: fn(Token) -> ParserError,
    ) -> Result<(), ParserError> {
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

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        match self.current_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => Err(ParserError::ExpectedSemicolon(Token::Assign)),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        let name;
        if let Token::Identifier(ident) = self.peek_token.clone() {
            self.next_token();
            name = ident;
        } else {
            return Err(ParserError::ExpectedIdentifierToken(
                self.peek_token.clone(),
            ));
        }

        self.expect_peek(Token::Assign, ParserError::ExpectedAssign)?;
        self.next_token();
        while self.current_token != Token::Semicolon {
            self.next_token();
        }

        let expr = Expression::IntegerLiteral(0);

        return Ok(Statement::Let(name, expr));
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        self.next_token();
        if self.current_token == Token::Semicolon {
            return Ok(Statement::Return(None));
        }

        while self.current_token != Token::Semicolon {
            self.next_token();
        }

        return Ok(Statement::Return(None));
    }
}

#[derive(Debug)]
pub enum ParserError {
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
    use crate::ast::{Expression, Statement};
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

    #[test]
    fn parse_let_stmt() {
        let input = "
        let x = 5;
        let y = 10.23;
        let foobar = 43243243;
        ";
        let lexer = Lexer::new(input.to_owned());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        let expected = vec![
            Statement::Let("x".to_string(), Expression::IntegerLiteral(5)),
            Statement::Let("y".to_string(), Expression::FloatLiteral(10.23)),
            Statement::Let("foobar".to_string(), Expression::IntegerLiteral(43243243)),
        ];

        check_parser_errors(&parser);
        assert_eq!(expected, program.statements,);
    }

    #[test]
    fn parse_return_stmt() {
        let input = "
        return;
        return 5;
        return 10.43;
        ";
        let lexer = Lexer::new(input.to_owned());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        let expected = vec![
            Statement::Return(None),
            Statement::Return(Some(Expression::IntegerLiteral(5))),
            Statement::Return(Some(Expression::FloatLiteral(10.43))),
        ];

        check_parser_errors(&parser);
        assert_eq!(expected, program.statements,);
    }
}
