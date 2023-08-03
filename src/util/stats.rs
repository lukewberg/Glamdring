use crate::types::{ScannerResult, Tokens};

pub fn report_scanning_statistics(scanner_result: &ScannerResult) {
    let mut num_punctuators = 0;
    let mut num_lexemes = 0;
    let mut num_identifiers = 0;
    let token_vec_len = scanner_result.token_iterator.len() as f64; // cast to float

    scanner_result
        .token_iterator
        .clone()
        .for_each(|token| match token.token_type {
            Tokens::Identifier => num_identifiers += 1,
            Tokens::OpenCurlyBrace
            | Tokens::CloseCurlyBrace
            | Tokens::OpenParenthesis
            | Tokens::CloseParenthesis
            | Tokens::OpenBrace
            | Tokens::CloseBrace
            | Tokens::Semicolon
            | Tokens::Comma
            | Tokens::Dot
            | Tokens::Colon => num_punctuators += 1,
            _ => num_lexemes += 1,
        });

    let id_perc = ((num_identifiers as f64 / token_vec_len) * 100.0) as i32;
    let punc_perc = ((num_punctuators as f64 / token_vec_len) * 100.0) as i32;
    let lex_perc = ((num_lexemes as f64 / token_vec_len) * 100.0) as i32;

    println!(
        "-- Scanner stats --\nIdentifiers: {} {}%\nPunctuators: {} {}%\nKeywords+Operators: {} {}%",
        num_identifiers, id_perc, num_punctuators, punc_perc, num_lexemes, lex_perc
    );
}
