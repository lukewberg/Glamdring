use std::{collections::HashMap, ops::Range};

use crate::types::{NumberType, ScanError, ScannerResult, ScannerState, Token, Tokens};

pub struct Lexer {
    token_map: HashMap<&'static str, Tokens>,
    char_token_map: HashMap<char, Tokens>,
}

impl<'a> Lexer {
    fn build_token_map() -> HashMap<&'static str, Tokens> {
        HashMap::from([
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
            ("Infinity", Tokens::Infinity),
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
        ])
    }

    fn build_char_token_map() -> HashMap<char, Tokens> {
        HashMap::from([
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
        ])
    }

    // fn build_number_token_map() -> HashMap<NumberType, Tokens> {
    //     HashMap::from([
    //         (NumberType::Int, Tokens::Int),
    //         (NumberType::Float, Tokens::Float),
    //         (NumberType::Hex, Tokens::Hex),
    //         (NumberType::Octal, Tokens::Octal),
    //         (NumberType::Binary, Tokens::Binary),
    //         (NumberType::Exponential, Tokens::Exponential),
    //         (NumberType::BigInt, Tokens::BigInt),
    //         (NumberType::BigHex, Tokens::BigHex),
    //         (NumberType::BigBinary, Tokens::BigBinary),
    //         (NumberType::BigOctal, Tokens::BigOctal),
    //     ])
    // }

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

                    match scanner_state {
                        ScannerState::InComment
                        | ScannerState::InBlockComment
                        | ScannerState::InStringDouble
                        | ScannerState::InStringSingle
                        | ScannerState::InStringTemplate => continue,
                        ScannerState::InWhitespace => {
                            start = i;
                            continue;
                        }
                        ScannerState::InNumber => {
                            // Exiting a number
                            // Can't validate number or get it's type without reaching a whitespace, newline, or operator
                        }
                        _ => {
                            // We have encountered white space
                            // Check to see if lexeme between `start` and `i`
                            if source_chars[start] == '\n' {
                                start = i;
                                scanner_state = ScannerState::Idle;
                                continue;
                            }
                            token_vec.push(self.get_token(&source_chars, start..i).unwrap_or_else(
                                || {
                                    Token::new(
                                        start..i,
                                        Tokens::Identifier,
                                        Some(source_chars[start..i].iter().collect()),
                                    )
                                },
                            ));
                            // Since we are in white space, set this to true in case of contiguous
                            scanner_state = ScannerState::InWhitespace;
                        }
                    }

                    start = i;
                    continue;
                }
                '/' => {
                    // Could be a division token or the start/end of a comment/block comment
                    // Check if last char was another `/`
                    if i == 0 {
                        continue;
                    }
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
                '\n' | '\r' => {
                    // Although it's uncommon, statements can be multiline
                    // Newlines do delimit tokens
                    // Newlines terminate single-line comments

                    match scanner_state {
                        ScannerState::InStringDouble | ScannerState::InStringSingle => {
                            // Strings are't multiline outside of template strings
                            // Throw error here
                            return Err(ScanError::InvalidSyntax {
                                token: String::from("/\n"),
                                location: i,
                            });
                        }
                        ScannerState::InComment => {
                            // We were in a comment but have reached it's end, change to `Idle` state
                            scanner_state = ScannerState::Idle;
                        }
                        ScannerState::InBlockComment | ScannerState::Idle => {
                            start = i;
                            continue;
                        }
                        _ => {
                            // Because newline delimits tokens, capture and add token to result vec
                            if let Some(token) = self.get_token(&source_chars, start..(i - 1)) {
                                token_vec.push(token);
                            }
                        }
                    }

                    // Scanner state is always idle when moving through newlines
                    scanner_state = ScannerState::Idle;
                    start = i;
                    continue;
                }
                '*' => {
                    // Asterisk can be an operator, part of an operator or signify the start of a block comment (if preceded by a `/`)
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
                        start = i;
                        continue;
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
                        ScannerState::InStringSingle | ScannerState::InStringDouble => {
                            // Check to see if we're at the end of the string
                            // String ends with same char as it began, either ' or "
                            // Also check for escape backslash
                            if (*char == '"' || *char == '\'')
                                && (scanner_state == ScannerState::InStringDouble
                                    || scanner_state == ScannerState::InStringSingle)
                            {
                                // Check for escape at previous index
                                // Only check if the escaped char is the same as what started the string
                                if (*char == '"' && scanner_state == ScannerState::InStringDouble
                                    || *char == '\''
                                        && scanner_state == ScannerState::InStringSingle)
                                    && source_chars[i - 1] == '\\'
                                {
                                    // We are still in the string, continue
                                    continue;
                                }
                                token_vec.push(Token::new(
                                    start..i,
                                    Tokens::String,
                                    Some(source_chars[start..i + 1].iter().collect()),
                                ));
                                scanner_state = ScannerState::Idle;
                                start = i;
                                continue;
                            }
                        }
                        _ => {
                            if *char == '"' && scanner_state != ScannerState::InStringDouble {
                                scanner_state = ScannerState::InStringDouble;
                                start = i;
                                continue;
                            } else if *char == '\'' && scanner_state != ScannerState::InStringSingle
                            {
                                scanner_state = ScannerState::InStringSingle;
                                start = i;
                                continue;
                            }

                            // Is current char a lexeme?
                            if let Some(_) = self.char_token_map.get(char) {
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
                                // At this point, we can't know if we're in a keyword, identifier or a number. Must test
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
                            // Check for number
                            if let Ok(_) = char.to_string().parse::<i32>() {
                                // all number types in JS at least start with a number
                                if scanner_state != ScannerState::InNumber {
                                    scanner_state = ScannerState::InNumber;
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
        unimplemented!();
    }

    #[inline]
    fn get_token(&self, source: &Vec<char>, range: Range<usize>) -> Option<Token> {
        // Check `token_map` to see if string is a keyword or operator
        let mut lexeme = String::with_capacity(source.len());
        for c in &source[range.start..range.end] {
            lexeme.push(*c);
        }
        if lexeme.len() > 1 {
            if let Some(token) = self.token_map.get(lexeme.as_str()) {
                // We have found a keyword/operator, now add it to the result vector
                // keywords/operators do not need to have their literal copied into the token
                Some(Token::new(range, token.clone(), None))
            } else {
                None
            }
        } else {
            // Check for single-char lexeme
            let char = lexeme.chars().next().unwrap_or_default();
            if let Some(token) = self.char_token_map.get(&char) {
                // keywords/operators do not need to have their literal copied into the token
                Some(Token::new(range, token.clone(), None))
            } else {
                None
            }
        }
    }

    fn get_number_type(&self, source: &Vec<char>, range: &Range<usize>) -> Option<NumberType> {
        // Get string of the first couple chars
        let literal = match source.get(range.start..range.end) {
            Some(result) => result,
            None => {
                return None;
            }
        };
        if let Some(array) = source.get(0..2) {
            let last_char = *(literal.last().unwrap());
            match array {
                ['0', 'x'] | ['0', 'X'] => {
                    // Hex
                    if last_char == 'n' {
                        // BigHex
                        return Some(NumberType::BigHex);
                    }
                    return Some(NumberType::Hex);
                }
                ['0', 'b'] | ['0', 'B'] => {
                    // Binary
                    if last_char == 'n' {
                        // BigBinary
                        return Some(NumberType::BigBinary);
                    }
                    return Some(NumberType::Binary);
                }
                ['0', 'o'] | ['0', 'O'] => {
                    // Octal
                    if last_char == 'n' {
                        return Some(NumberType::BigOctal);
                    }
                    return Some(NumberType::Octal);
                }
                _ => {
                    // Exponential have n digits followed by 'e' followed by an optional +- and then digits
                    // BigInt only ends with 'n', cannot start with 0
                    if last_char == 'n' {
                        return Some(NumberType::BigInt);
                    }
                }
            }
        }
        None
    }

    fn get_number_token(&self, source: &Vec<char>, range: Range<usize>) -> Option<Token> {
        // Validate number and get it's type
        // Array is on the stack, BLAZINGLY fast
        let token_type_array = [
            Tokens::Float,
            Tokens::Int,
            Tokens::Hex,
            Tokens::Octal,
            Tokens::Binary,
            Tokens::Exponential,
            Tokens::BigInt,
            Tokens::BigHex,
            Tokens::BigBinary,
            Tokens::BigOctal,
        ];

        if let Some(number_type) = self.get_number_type(&source, &range) {
            let literal: String = source[range.start..range.end].iter().collect();
            return Some(Token::new(
                range,
                token_type_array[number_type as usize].to_owned(),
                Some(literal),
            ));
        }
        None
    }

    fn is_number_exponential(source: &Vec<char>, range: Range<usize>) -> bool {
        let mut e_found = false;
        let mut sign_found = false;
        for &char in source[range].iter() {
            // If we have already encountered 'e', the rest must be numbers or +/-
            // If not, continue to parse numbers.
            if let Ok(_) = char.to_string().parse::<i32>() {
            } else if char == '+' || char == '-' {
                if sign_found == true {
                    // Invalid, return false
                    return false;
                }
                sign_found = true;
            } else if char == 'e' || char == 'E' {
                if e_found == true {
                    // Invalid, return false
                    return false;
                }
                e_found = true;
            } else {
                return false;
            }
        }
        false
    }

    pub fn new() -> Lexer {
        Lexer {
            token_map: Self::build_token_map(),
            char_token_map: Self::build_char_token_map(),
        }
    }
}
