use std::io::{self, Write};
extern crate crab_lib;

use crab_lib::{evaluator::evaluator, lexer::lexer::Lexer, parser::parser::Parser};

use crate::{ferris_str, parse_err_fmt_str};

pub fn start() {
    loop {
        let input = ask_input(">>");
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        if !parser.errors().is_empty() {
            let errs: String = parse_err_fmt_str(parser.errors());
            println!("{}", ferris_str(errs));
            continue;
        }

        let evaluated = evaluator::eval(&program);
        match evaluated {
            Ok(res) => {
                println!("{:#?}", res)
            }
            Err(err) => {
                let error_msg = format!(
                    "Oh Crab! I encountered an error during evaluation:\n{:?}",
                    err
                );
                println!("{}", ferris_str(error_msg));
                continue;
            }
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
    input
}
