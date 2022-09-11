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

pub fn ferris_str(input: String) -> String {
    let mut vec = Vec::new();
    say(input.as_bytes(), 200, &mut vec).unwrap();
    let actual = std::str::from_utf8(&vec).unwrap();
    actual.to_string()
}
