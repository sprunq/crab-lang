use self::token::{lookup_ident, Token};

pub mod tests;
pub mod token;

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
