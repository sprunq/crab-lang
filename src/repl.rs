use std::io::{self, Write};
extern crate crab_lib;
use crab_lib::{lexer::Lexer, parser::Parser};

use crate::print_parser_errors_fmt_string;

pub fn start() {
    loop {
        let input = ask_input(">>");
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        if parser.errors().len() > 0 {
            let errs: String = print_parser_errors_fmt_string(parser.errors());
            println!("{}", errs);
            continue;
        }

        println!("{}", program);
        println!("{}", "\n")
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
