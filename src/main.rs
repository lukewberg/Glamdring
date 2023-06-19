use std::{env, fs, time::Instant};
use weblex::{lexer::lexer::Lexer, util::stats::report_scanning_statistics};

fn main() {
    // Collect command-line arguments
    let args: Vec<String> = env::args().collect();
    let file_path = &args.get(1).expect("No input supplied, aborting!");
    // println!("Path: {file_path}");

    let file_string = fs::read_to_string(file_path).expect("Unable to read file!");
    let token_hashmap = Lexer::build_token_map();
    let char_token_map = Lexer::build_char_operator_token_map();
    let lexer = Lexer::new(&token_hashmap, &char_token_map);
    let now = Instant::now();
    match lexer.scan(&file_string) {
        Ok(_result) => {
            println!("{:#?}", _result);
            println!("Scanned in: {:.2?}", now.elapsed());
            report_scanning_statistics(&_result);
        }
        Err(error) => {
            println!("Error while scanning: {:#?}", error)
        }
    }
}
