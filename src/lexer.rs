use crate::token::{lookup_ident, Token};

pub struct Lexer {
    pub input: String,
    position: usize,      // current position in input (points to current char)
    read_position: usize, // current reading position in input (after current char)
    character: char,      // current char under examination
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            character: '\u{0}',
        };
        lexer.read_char();
        return lexer;
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let tok: Token;
        match self.character {
            '+' => tok = Token::Plus,
            '-' => tok = Token::Minus,
            '/' => tok = Token::Slash,
            '*' => tok = Token::Asterisk,
            '<' => tok = Token::Lt,
            '>' => tok = Token::Gt,
            ';' => tok = Token::Semicolon,
            ',' => tok = Token::Comma,
            '(' => tok = Token::LParenthesis,
            ')' => tok = Token::RParenthesis,
            '{' => tok = Token::LBrace,
            '}' => tok = Token::RBrace,
            '\0' => tok = Token::Eof,
            '=' => {
                tok = {
                    if self.peek_char() == '=' {
                        self.read_char();
                        Token::Equal
                    } else {
                        Token::Assign
                    }
                };
            }
            '!' => {
                tok = {
                    if self.peek_char() == '=' {
                        self.read_char();
                        Token::NotEqual
                    } else {
                        Token::Bang
                    }
                };
            }
            _ => {
                if Self::is_letter(self.character) {
                    let ident = self.read_identifier();
                    return lookup_ident(&ident);
                } else if Self::is_digit(self.character) {
                    let digit = self.read_number();
                    return Token::Int(digit);
                } else {
                    tok = Token::Illegal
                }
            }
        };
        self.read_char();
        tok
    }

    fn skip_whitespace(&mut self) {
        while self.character.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn is_digit(character: char) -> bool {
        character.is_ascii_digit()
    }

    fn is_letter(character: char) -> bool {
        character.is_alphabetic() || character == '_'
    }

    pub fn read_identifier(&mut self) -> String {
        let start_pos = self.position;
        while Self::is_letter(self.character) {
            self.read_char();
        }
        self.input[start_pos..self.position].to_string()
    }

    fn read_number(&mut self) -> String {
        let start_pos = self.position;
        while Self::is_digit(self.character) {
            self.read_char();
        }
        self.input[start_pos..self.position].to_string()
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.character = '\0';
        } else {
            self.character = self.input.chars().nth(self.read_position).unwrap_or('\0');
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&mut self) -> char {
        if self.read_position >= self.input.len() {
            return '\0';
        } else {
            return self.input.chars().nth(self.read_position).unwrap_or('\0');
        }
    }
}

#[cfg(test)]
pub mod texer_test {
    use crate::{lexer::Lexer, token::Token};

    fn assert_lex_against(input: &str, expected: Vec<Token>) {
        let mut lexer = Lexer::new(input.to_owned());
        for expected_token in expected.iter() {
            let actual_token = lexer.next_token();
            assert_eq!(
                expected_token, &actual_token,
                "expected {} found {}",
                expected_token, actual_token
            );
        }
    }

    #[test]
    fn test_var_lex() {
        let input = r#"
        let five = 5;
        let ten = 10;
        "#;
        let expected = vec![
            Token::Let,
            Token::Identifier("five".to_string()),
            Token::Assign,
            Token::Int(5.to_string()),
            Token::Semicolon,
            Token::Let,
            Token::Identifier("ten".to_string()),
            Token::Assign,
            Token::Int(10.to_string()),
            Token::Semicolon,
        ];
        assert_lex_against(input, expected);
    }

    #[test]
    fn test_operator_lex() {
        let input = r#"
        +-!/*5;
        5 < 10 > 5;
        "#;
        let expected = vec![
            Token::Plus,
            Token::Minus,
            Token::Bang,
            Token::Slash,
            Token::Asterisk,
            Token::Int(5.to_string()),
            Token::Semicolon,
            Token::Int(5.to_string()),
            Token::Lt,
            Token::Int(10.to_string()),
            Token::Gt,
            Token::Int(5.to_string()),
            Token::Semicolon,
        ];

        assert_lex_against(input, expected);
    }

    #[test]
    fn test_double_char_operators() {
        let input = r#"
        10 == 10;
        10 != 9;
        "#;
        let expected = vec![
            Token::Int(10.to_string()),
            Token::Equal,
            Token::Int(10.to_string()),
            Token::Semicolon,
            Token::Int(10.to_string()),
            Token::NotEqual,
            Token::Int(9.to_string()),
            Token::Semicolon,
        ];

        assert_lex_against(input, expected);
    }

    #[test]
    fn test_if_else_lex() {
        let input = r#"
        if (5 < 10) {
            return true;
        } else {
            return false;
        }
        "#;
        let expected = vec![
            Token::If,
            Token::LParenthesis,
            Token::Int(5.to_string()),
            Token::Lt,
            Token::Int(10.to_string()),
            Token::RParenthesis,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,
        ];

        assert_lex_against(input, expected);
    }

    #[test]
    fn test_function_lex() {
        let input = r#"
        function add(x, y){
            return x + y;
        };
        let result = add(five, ten);
        "#;
        let expected = vec![
            Token::Function,
            Token::Identifier("add".to_string()),
            Token::LParenthesis,
            Token::Identifier("x".to_string()),
            Token::Comma,
            Token::Identifier("y".to_string()),
            Token::RParenthesis,
            Token::LBrace,
            Token::Return,
            Token::Identifier("x".to_string()),
            Token::Plus,
            Token::Identifier("y".to_string()),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Identifier("result".to_string()),
            Token::Assign,
            Token::Identifier("add".to_string()),
            Token::LParenthesis,
            Token::Identifier("five".to_string()),
            Token::Comma,
            Token::Identifier("ten".to_string()),
            Token::RParenthesis,
            Token::Semicolon,
        ];

        assert_lex_against(input, expected);
    }
}
