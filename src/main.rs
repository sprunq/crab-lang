pub mod ast;
pub mod lexer;
pub mod parse_error;
pub mod parser;
pub mod repl;
pub mod token;

fn main() {
    println!("Running..");
    repl::start();
}
