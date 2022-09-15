use std::{cell::RefCell, fs, rc::Rc};
extern crate crab_lib;
use std::time::Instant;

use crab_lib::{
    evaluator::evaluator,
    lexer::lexer::Lexer,
    object::{environment::Environment, object::Object},
    parser::parser::Parser,
};

use crate::ferris_str;

pub fn start(path: &str) {
    let data = fs::read_to_string(path).expect("Unable to read file");
    let lexer = Lexer::new(data.clone());
    let mut parser = Parser::new(lexer);
    let parse_res = parser.parse_program();
    if let Err(ref err) = parse_res {
        let error_msg = format!("Oh Crab! I encountered an error during parsing:\n{:?}", err);
        println!("{}", ferris_str(error_msg));
        std::process::exit(0);
    }

    let now = Instant::now();
    let env = Rc::new(RefCell::new(Environment::new()));
    let evaluated = evaluator::eval(&parse_res.unwrap(), Rc::clone(&env));
    let elapsed = now.elapsed();
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
    println!("\nFinished evaluation in: {:.2?}", elapsed / 100000);
}
