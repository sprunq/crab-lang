use std::fs;
extern crate crab_lib;

use crab_lib::{lexer::lexer::Lexer, parser::parser::Parser};

use crate::{ferris_str, parse_err_fmt_str};

pub fn start(path: &str) {
    let data = fs::read_to_string(path).expect("Unable to read file");
    let lexer = Lexer::new(data.clone());
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    println!("Input: \n{}\n", data);
    println!("Parsed: \n{:#?}", program);

    if !parser.errors().is_empty() {
        let errs: String = parse_err_fmt_str(parser.errors());
        println!("{}", ferris_str(errs));
    }
}
