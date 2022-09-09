use std::io::{self, Write};
extern crate crab_lib;
use crab_lib::{
    lexer::Lexer,
    parser::{parse_error::ParseErr, Parser},
};
use ferris_says::say;

pub fn start() {
    println!("{}", "Welcome to crab-lang v0.1\n");
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

fn print_parser_errors_fmt_string(errors: &[ParseErr]) -> String {
    let errors = errors
        .iter()
        .map(|a| a.to_string())
        .collect::<Vec<String>>()
        .join(", \n- ");

    let input = format!(
        "Looks like I encountered some errors during parsing: \n- {}",
        errors
    );
    let mut vec = Vec::new();
    say(input.as_bytes(), 100, &mut vec).unwrap();
    let actual = std::str::from_utf8(&vec).unwrap();
    return actual.to_string();
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
