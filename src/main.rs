use clap::{App, Arg, Parser};
use ferris_says::say;
use std::fmt;
pub mod interpret_file;
pub mod repl;

fn main() {
    let matches = App::new("crab-lang")
        .version("1.0")
        .about("The crab language interpreter")
        .arg(
            Arg::with_name("FILE_PATH")
                .short('f')
                .long("file")
                .help("The file to interpret")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("MEASURE_PERFORMANCE")
                .short('t')
                .long("time")
                .help("Measures the execution time of the different stages")
                .takes_value(false)
                .default_value("false"),
        )
        .get_matches();

    let path = matches.value_of("FILE_PATH").unwrap_or("").to_lowercase();
    let measure_perf = matches
        .value_of("MEASURE_PERFORMANCE")
        .unwrap_or("false")
        .parse::<bool>()
        .unwrap_or(false);

    if path.is_empty() {
        repl::start()
    } else {
        interpret_file::start(&path, measure_perf)
    }
}
