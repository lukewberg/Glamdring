use std::env;
use std::fs;
use std::io;

fn main() {
    // Collect command-line arguments
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];
    println!("Path: {file_path}");

    let file_string = fs::read_to_string(file_path);
    match file_string {
        Result::Ok(result) => {
            print!("{result}");
        }
        Result::Err(error) => {
            println!("{:#?}", error);
        }
    };
}
