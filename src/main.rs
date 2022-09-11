use crab_lib::parser::parse_error::ParseErr;
use ferris_says::say;

pub mod interpret_file;
pub mod repl;

#[allow(dead_code)]
enum RunMode {
    Repl,
    InterpretFile,
}

fn main() {
    println!("Welcome to crab-lang v0.1\n");
    let run_mode = RunMode::Repl;
    match run_mode {
        RunMode::Repl => repl::start(),
        RunMode::InterpretFile => interpret_file::start("res/input.crab"),
    };
}

pub fn parse_err_fmt_str(errors: &[ParseErr]) -> String {
    let errors_fmt = errors
        .iter()
        .map(|a| a.to_string())
        .collect::<Vec<String>>()
        .join(", \n- ");

    format!(
        "{}\n- {}",
        "Looks like I encountered some errors during parsing: ", errors_fmt
    )
}

pub fn ferris_str(input: String) -> String {
    let mut vec = Vec::new();
    say(input.as_bytes(), 200, &mut vec).unwrap();
    let actual = std::str::from_utf8(&vec).unwrap();
    actual.to_string()
}
