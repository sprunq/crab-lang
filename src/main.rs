use clap::Parser;
use ferris_says::say;
use std::fmt;
pub mod interpret_file;
pub mod repl;

fn main() {
    let args = Args::parse();
    println!("Welcome to crab-lang v0.1\n");

    match args.run_mode {
        RunMode::Repl => repl::start(),
        RunMode::File => interpret_file::start(&args.path),
    };
}

pub fn ferris_str(input: String) -> String {
    let mut vec = Vec::new();
    say(input.as_bytes(), 200, &mut vec).unwrap();
    let actual = std::str::from_utf8(&vec).unwrap();
    actual.to_string()
}

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// The path of the file to interpret (has to be in File mode)
    #[clap(short, long, value_parser, default_value = "examples/input.crab")]
    path: String,

    /// The mode to run in
    #[clap(short, long, value_parser, default_value_t = RunMode::File)]
    run_mode: RunMode,
}

#[derive(PartialEq, Debug, Clone, clap::ArgEnum)]
pub enum RunMode {
    Repl,
    File,
}

impl fmt::Display for RunMode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            RunMode::Repl => write!(f, "repl"),
            RunMode::File => write!(f, "file"),
        }
    }
}
