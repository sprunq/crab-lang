pub mod lexer;
pub mod repl;
pub mod token;

fn main() {
    println!("Running..");
    repl::start();
}
