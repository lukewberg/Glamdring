use core::time;
use std::{
    collections::HashMap,
    ops::Range,
    thread::{self},
};

use crate::types::{ScanError, ScannerResult, ScannerState, Token, Tokens};

pub struct JsLexer {
    token_map: HashMap<&'static str, Tokens>,
    char_token_map: HashMap<char, Tokens>,
}

impl<'a> JsLexer {
    fn build_token_map() -> HashMap<&'static str, Tokens> {
        let token_map = HashMap::from([
            ("function", Tokens::Function),
            ("await", Tokens::Await),
            ("break", Tokens::Break),
            ("case", Tokens::Case),
            ("catch", Tokens::Catch),
            ("class", Tokens::Class),
            ("const", Tokens::Const),
            ("continue", Tokens::Continue),
            ("debugger", Tokens::Debugger),
            ("default", Tokens::Default),
            ("delete", Tokens::Delete),
            ("do", Tokens::Do),
            ("else", Tokens::Else),
            ("enum", Tokens::Enum),
            ("export", Tokens::Export),
            ("extends", Tokens::Extends),
            ("false", Tokens::False),
            ("finally", Tokens::Finally),
            ("for", Tokens::For),
            ("function", Tokens::Function),
            ("if", Tokens::If),
            ("import", Tokens::Import),
            ("in", Tokens::In),
            ("instanceof", Tokens::Instanceof),
            ("new", Tokens::New),
            ("null", Tokens::Null),
            ("return", Tokens::Return),
            ("super", Tokens::Super),
            ("switch", Tokens::Switch),
            ("this", Tokens::This),
            ("throw", Tokens::Throw),
            ("true", Tokens::True),
            ("try", Tokens::Try),
            ("typeof", Tokens::Typeof),
            ("var", Tokens::Var),
            ("void", Tokens::Void),
            ("while", Tokens::While),
            ("with", Tokens::With),
            ("yield", Tokens::Yield),
            (".?", Tokens::OptionalChain),
            ("...", Tokens::Ellipsis),
            ("<=", Tokens::LessThanEqual),
            (">=", Tokens::GreaterThanEqual),
            ("==", Tokens::Equality),
            ("!=", Tokens::NotEqual),
            ("===", Tokens::StrictEqual),
            ("!==", Tokens::NotStrictEqual),
            ("**", Tokens::Exponentiation),
            ("++", Tokens::Increment),
            ("--", Tokens::Decrement),
            ("<<", Tokens::LeftShift),
            (">>", Tokens::RightShift),
            (">>>", Tokens::UnsignedRightShift),
            ("&&", Tokens::And),
            ("||", Tokens::Or),
            ("??", Tokens::NullishCoalescing),
            ("+=", Tokens::AdditionAssignment),
            ("-=", Tokens::SubtractionAssignment),
            ("*=", Tokens::MultiplicationAssignment),
            ("/=", Tokens::DivisionAssignment),
            ("%=", Tokens::RemainderAssignment),
            ("**=", Tokens::ExponentiationAssignment),
            ("<<=", Tokens::LeftShiftAssignment),
            (">>=", Tokens::RightShiftAssignment),
            (">>>=", Tokens::UnsignedRightShiftAssignment),
            ("&=", Tokens::BitwiseAndAssignment),
            ("|=", Tokens::BitwiseOrAssignment),
            ("^=", Tokens::BitwiseXorAssignment),
            ("&&=", Tokens::AndAssignment),
            ("||=", Tokens::OrAssignment),
            ("??=", Tokens::NullishCoalescingAssignment),
            ("=>", Tokens::ArrowNotation),
        ]);

        token_map
    }

    fn build_char_token_map() -> HashMap<char, Tokens> {
        let char_token_map = HashMap::from([
            ('{', Tokens::OpenCurlyBrace),
            ('}', Tokens::CloseCurlyBrace),
            ('(', Tokens::OpenParenthesis),
            (')', Tokens::CloseParenthesis),
            ('[', Tokens::OpenBrace),
            (']', Tokens::CloseBrace),
            ('.', Tokens::Dot),
            (';', Tokens::Semicolon),
            (',', Tokens::Comma),
            ('<', Tokens::LessThan),
            ('>', Tokens::GreaterThan),
            ('+', Tokens::UnaryPlus),
            ('-', Tokens::UnaryNegation),
            ('*', Tokens::Multiply),
            ('/', Tokens::Divide),
            ('%', Tokens::Modulo),
            ('&', Tokens::BitwiseAnd),
            ('|', Tokens::BitwiseOr),
            ('^', Tokens::BitwiseXor),
            ('!', Tokens::Bang),
            ('~', Tokens::BitwiseNot),
            ('?', Tokens::Ternary),
            (':', Tokens::Colon),
            ('=', Tokens::Assignment),
        ]);

        char_token_map
    }

    pub fn scan(&self, source: &str) -> Result<ScannerResult, ScanError> {
        let mut start = 0;

        /* When we encounter a single-char lexeme like =, we keep proceeding
        until we encounter a char that is not a single-char lexeme */

        // Machine states default to Idle
        let mut scanner_state = ScannerState::Idle;

        // Convert source string into a char vec so we can index into it
        let source_chars: Vec<char> = source.chars().collect();

        let mut token_vec: Vec<Token> = Vec::new();

        for (i, char) in source_chars.iter().enumerate() {
            // thread::sleep(time::Duration::from_millis(1200));
            match char {
                ' ' => {
                    // Whitespace can  terminate a lexeme, be contiguous, or also be part of a string or comment.
                    // When first encountering white-space we will check for a lexeme between the start and current range
                    // `scanner_state` will then be set to `ScannerState::InWhitespace`
                    // If `scanner_state` is already `ScannerState::InWhitespace`, we will only increment start until non-white space is encountered

                    // We could be traversing white space in a comment or string literal
                    // Increment current and continue if so
                    if scanner_state == ScannerState::InComment
                        || scanner_state == ScannerState::InBlockComment
                        || scanner_state == ScannerState::InString
                    {
                        continue;
                    } else if scanner_state == ScannerState::InWhitespace {
                        start = i;
                        continue;
                    } else {
                        // We have encountered white space
                        // Check to see if lexeme between `start` and `i`
                        token_vec.push(self.get_token(&source_chars, start..i).unwrap_or_else(
                            || {
                                Token::new(
                                    start..i,
                                    Tokens::Identifier,
                                    source_chars[start..i].iter().collect(),
                                )
                            },
                        ));
                        // Since we are in white space, set this to true in case of contiguous
                        scanner_state = ScannerState::InWhitespace;

                        start = i;
                    }
                }
                '/' => {
                    // Could be a division token or the start/end of a comment/block comment
                    // Check if last char was another `/`
                    if source_chars[i - 1] == '/' {
                        // We are in a comment, change machine state
                        scanner_state = ScannerState::InComment;
                    } else if source_chars[i - 1] == '*' {
                        // We have exited a block comment
                        scanner_state = ScannerState::Idle;
                    } else {
                        // If we aren't in a comment, proceed as normal operator and catch in another match arm
                        scanner_state = ScannerState::InOperator;
                    }
                }
                '\n' => {
                    // Although it's uncommon, statements can be multiline
                    // Newlines do delimit tokens
                    // Newlines terminate single-line comments
                    if scanner_state == ScannerState::InComment {
                        // We were in a comment but have reached it's end, change to `Idle` state
                        scanner_state = ScannerState::Idle;
                    }

                    match scanner_state {
                        ScannerState::Idle => todo!(),
                        ScannerState::InIdentifier => todo!(),
                        ScannerState::InOperator => todo!(),
                        ScannerState::InNumber => todo!(),
                        ScannerState::InString => todo!(),
                        ScannerState::InComment => todo!(),
                        ScannerState::InBlockComment => todo!(),
                        ScannerState::InWhitespace => todo!(),
                    }

                    start = i;
                    continue;
                }
                '*' => {
                    // Asterisk can be an operator or signify the start of a block comment (if preceded by a `/`)
                    if scanner_state == ScannerState::InOperator {
                        // If we find a preceding '/' char, we're in a block comment
                        if *&source_chars[i - 1] == '/' {
                            scanner_state = ScannerState::InBlockComment;
                            continue;
                        }
                    }
                    if source_chars[i - 1] == '/' {
                        scanner_state = ScannerState::InBlockComment;
                    } else {
                        // If no '/' char is behind us, we must be part of a normal operator. Will catch in another match arm
                        scanner_state = ScannerState::InOperator;
                    }
                }
                '`' => {
                    // TODO: Call `scan_template_string` here
                }
                _ => {
                    // Catch-all for the rest of the lexemes
                    match scanner_state {
                        ScannerState::InComment | ScannerState::InBlockComment => {
                            continue;
                        }
                        _ => {
                            if *char == '"' && scanner_state != ScannerState::InString {
                                scanner_state = ScannerState::InString;
                                start = i;
                            } else if *char == '"' {
                                token_vec.push(Token::new(
                                    start..i,
                                    Tokens::String,
                                    source_chars[start..i].iter().collect(),
                                ));
                                scanner_state = ScannerState::Idle;
                            }

                            // Is current char a lexeme?
                            if let Some(result_char) = self.char_token_map.get(char) {
                                if scanner_state != ScannerState::InOperator {
                                    start = i;
                                } else if scanner_state == ScannerState::InIdentifier {
                                    // If we were in an identifier, we need to close that out
                                    token_vec
                                        .push(self.get_token(&source_chars, start..i).unwrap());
                                }
                                scanner_state = ScannerState::InOperator;
                                continue;
                            } else {
                                // White space is caught above. The char is not a lexeme itself (not an operator)
                                // At this point, we can't know if we're in a keyword or an identifier, identifier state will cover both
                                if scanner_state != ScannerState::InIdentifier {
                                    // Just exited an operator, check for token between range
                                    if let Some(token) = self.get_token(&source_chars, start..i) {
                                        // There's a token, add to vec
                                        token_vec.push(token);
                                    }
                                    scanner_state = ScannerState::InIdentifier;
                                    start = i;
                                    continue;
                                }
                            }
                        }
                    }
                }
            }
            // println!("Scanner state is: {:#?}\n on char: {}", scanner_state, char);
        }
        Ok(ScannerResult {
            file_name: String::from("Hello World!"),
            token_vec,
        })
    }

    // fn peek(&self, target: char, index: usize) -> bool {
    //     if index < self.source.len() {
    //         self.source.chars().nth(index).unwrap() == target
    //     } else {
    //         false
    //     }
    // }

    fn scan_template_string(source: &Vec<char>) -> () {
        // TODO: Implement
        ()
    }

    fn get_token(&self, source: &Vec<char>, range: Range<usize>) -> Option<Token> {
        // Check `token_map` to see if string is a keyword or operator
        let lexeme: String = source[range.to_owned()].iter().collect();
        if let Some(token) = self.token_map.get(&lexeme[..]) {
            // We have found a keyword/operator, now add it to the result vector
            Some(Token::new(range, token.clone(), lexeme))
        } else {
            None
        }
    }

    pub fn new() -> JsLexer {
        JsLexer {
            token_map: Self::build_token_map(),
            char_token_map: Self::build_char_token_map(),
        }
    }
}
