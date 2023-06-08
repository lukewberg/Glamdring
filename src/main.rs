use std::{env, fs, time::Instant};
use weblex::lexer::lexer::Lexer;

fn main() {
    // Collect command-line arguments
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];
    println!("Path: {file_path}");

    let file_string = fs::read_to_string(file_path).expect("Unable to read file!");
    let lexer = Lexer::new();
    let now = Instant::now();
    match lexer.scan(&file_string) {
        Ok(result) => println!("{:#?}", result),
        Err(error) => {
            println!("Error while scanning: {:#?}", error)
        }
    }
    println!("Elapsed: {:.2?}", now.elapsed());
}
