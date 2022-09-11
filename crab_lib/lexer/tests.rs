#[cfg(test)]
pub mod tests {
    use crate::lexer::{lexer::Lexer, token::Token};

    fn assert_lex_against(input: &str, expected: Vec<Token>) {
        let mut lexer = Lexer::new(input.to_owned());
        for expected_token in expected.iter() {
            let actual_token = lexer.next_token().0;
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
