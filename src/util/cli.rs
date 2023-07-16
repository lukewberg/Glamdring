use std::{env, error::Error};

use crate::types::CliArgs;

pub fn get_cli_args() -> Result<Vec<CliArgs>, Box<dyn Error>> {
    let args = env::args().map(|item| {
        match item.split(' ').collect::<Vec<&str>>()[0] {
            "-o" | "-O" => (),
            _ => ()
        }
    });
    Ok(vec![])
}