use std::{cell::RefCell, fs, rc::Rc};
extern crate crab_lib;
use std::time::Instant;

use crab_lib::{
    evaluator::evaluator,
    lexer::lexer::Lexer,
    object::{environment::Environment, object::Object},
    parser::parser::Parser,
};
use ferris_says::say;

pub fn start(path: &str, measure_time: bool) {
    let data = fs::read_to_string(path).expect("Unable to read file");
    let lexer = Lexer::new(data.clone());
    let mut parser = Parser::new(lexer);

    let mut now = Instant::now();
    let parse_res = parser.parse_program();
    let parsing_elapsed = now.elapsed();

    if let Err(ref err) = parse_res {
        let error_msg = format!("Oh Crab! I encountered an error during parsing:\n{:?}", err);
        println!("{}", ferris_str(error_msg));
        std::process::exit(0);
    }

    now = Instant::now();
    let env = Rc::new(RefCell::new(Environment::new()));
    let evaluated = evaluator::eval(&parse_res.unwrap(), Rc::clone(&env));
    let evaluation_elapsed = now.elapsed();

    match evaluated {
        Ok(res) => {
            if Object::Null != res {
                println!("{}", res)
            }
        }
        Err(err) => {
            let error_msg = format!(
                "Oh Crab! I encountered an error during evaluation:\n{:?}",
                err
            );
            println!("{}", ferris_str(error_msg));
            std::process::exit(0);
        }
    }

    if measure_time {
        println!("\nExection times:");
        println!("Parsing time: {:.2?}", parsing_elapsed);
        println!("Evaluation time: {:.2?}", evaluation_elapsed);
    }
}

pub fn ferris_str(input: String) -> String {
    let mut vec = Vec::new();
    say(input.as_bytes(), 200, &mut vec).unwrap();
    let actual = std::str::from_utf8(&vec).unwrap();
    actual.to_string()
}
