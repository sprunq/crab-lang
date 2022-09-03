use std::io::{self, Write};

use crate::{lexer::Lexer, token::Token};

pub fn start() {
    loop {
        let input = ask_input(">>");
        let mut lexer = Lexer::new(input);
        let mut token = lexer.next_token();
        while !token.eq(&Token::Eof) {
            println!("Token:{}", token);
            token = lexer.next_token();
        }
    }
}

fn ask_input(prompt: &str) -> String {
    let mut input = String::new();
    print!("{}", prompt);
    io::stdout().flush().expect("Failed to flush stdout");
    io::stdin()
        .read_line(&mut input)
        .expect("Failed to read line from stdin");
    return input;
}
