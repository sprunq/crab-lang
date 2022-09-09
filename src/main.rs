use std::fs;

use crab_lib::{
    lexer::Lexer,
    parser::{parse_error::ParseErr, Parser},
};
use ferris_says::say;

pub mod repl;

fn main() {
    println!("{}", "Welcome to crab-lang v0.1\n");
    if false {
        repl::start();
    } else {
        let data = fs::read_to_string("res/input.crab").expect("Unable to read file");
        let lexer = Lexer::new(data.clone());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        if parser.errors().len() > 0 {
            let errs: String = print_parser_errors_fmt_string(parser.errors());
            println!("{}", errs);
        }

        println!("Input: \n{}\n", data);
        println!("Parsed: \n{}", program);
    }
}

pub fn print_parser_errors_fmt_string(errors: &[ParseErr]) -> String {
    let errors_fmt = errors
        .iter()
        .map(|a| a.to_string())
        .collect::<Vec<String>>()
        .join(", \n- ");

    let input = format!(
        "{}\n- {}",
        "Looks like I encountered some errors during parsing: ", errors_fmt
    );
    let mut vec = Vec::new();
    say(input.as_bytes(), 200, &mut vec).unwrap();
    let actual = std::str::from_utf8(&vec).unwrap();
    return actual.to_string();
}
