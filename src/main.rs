use std::{env, fs, time::Instant};
use weblex::{lexer::lexer::Lexer, util::stats::report_scanning_statistics};

// fn main() {
//     let time = Instant::now();
//     for i in 0..1_000_000_000 {
//         continue;
//     }
//     println!("{:.2?}", time.elapsed());
// }

fn main() -> Result<(), usize> {
    // Collect command-line arguments
    let args: Vec<String> = env::args().collect();
    let file_path = args.get(1).expect("No input supplied, aborting!");
    // println!("Path: {file_path}");

    let file_string = fs::read_to_string(file_path).expect("Unable to read file!");
    let (token_hashmap, char_token_hashmap, char_punctuator_hashmap) = Lexer::get_maps();

    let lexer = Lexer::new(
        &token_hashmap,
        &char_token_hashmap,
        &char_punctuator_hashmap,
    );
    let now = Instant::now();
    match lexer.scan(&file_string) {
        Ok(_result) => {
            // println!("{:#?}", _result.token_iterator);
            println!("Scanned in: {:.2?}", now.elapsed());
            report_scanning_statistics(&_result);
        }
        Err(error) => {
            println!("Error while scanning: {:#?}", error)
        }
    }
    Ok(())
}
