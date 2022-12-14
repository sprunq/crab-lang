use clap::{App, Arg};
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
                .required(false),
        )
        .get_matches();

    let path = matches.value_of("FILE_PATH").unwrap_or("").to_lowercase();
    let measure_perf = matches.is_present("MEASURE_PERFORMANCE");

    if path.is_empty() {
        repl::start()
    } else {
        interpret_file::start(&path, measure_perf)
    }
}
