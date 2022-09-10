use std::fs;
extern crate crab_lib;
use crab_lib::{lexer::Lexer, parser::Parser};

use crate::{ferris_str, parse_err_fmt_str};

pub fn start() {
    let data = fs::read_to_string("res/input.crab").expect("Unable to read file");
    let lexer = Lexer::new(data.clone());
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    println!("Input: \n{}\n", data);
    println!("Parsed: \n{}", program);

    if parser.errors().len() > 0 {
        let errs: String = parse_err_fmt_str(parser.errors());
        println!("{}", ferris_str(errs));
    }
}
