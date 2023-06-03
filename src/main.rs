use std::{env, fs};
use weblex::lexer::js_lexer::JsLexer;

fn main() {
    // Collect command-line arguments
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];
    println!("Path: {file_path}");

    let file_string = fs::read_to_string(file_path).expect("Unable to read file!");
    let lexer = JsLexer::new();
    match lexer.scan(&file_string) {
        Ok(result) => println!("{:#?}", result),
        Err(error) => {
            println!("Error while scanning: {:#?}", error)
        }
    }
}
