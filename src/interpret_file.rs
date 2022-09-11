use std::fs;
extern crate crab_lib;

use crab_lib::{lexer::lexer::Lexer, parser::parser::Parser};

use crate::ferris_str;

pub fn start(path: &str) {
    let data = fs::read_to_string(path).expect("Unable to read file");
    let lexer = Lexer::new(data.clone());
    let mut parser = Parser::new(lexer);
    let parse_res = parser.parse_program();
    if let Err(ref err) = parse_res {
        let error_msg = format!("Oh Crab! I encountered an error during parsing:\n{:?}", err);
        println!("{}", ferris_str(error_msg));
    }

    println!("Input: \n{}\n", data);
    println!("Parsed: \n{:#?}", parse_res.unwrap());
}
