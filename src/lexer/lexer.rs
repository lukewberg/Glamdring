use std::{iter::Peekable, mem::size_of, ops::Range, str::CharIndices};

use ahash::AHashMap;

use crate::types::{NumberType, ScanError, ScannerResult, Token, Tokens};

pub struct Lexer<'a> {
    token_map: &'a AHashMap<&'static str, Tokens>,
    char_operator_token_map: &'a AHashMap<char, Tokens>,
    char_punctuator_token_map: &'a AHashMap<char, Tokens>,
}

impl<'a> Lexer<'a> {
    pub fn build_token_map() -> AHashMap<&'static str, Tokens> {
        AHashMap::from([
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
            ("let", Tokens::Let),
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

    pub fn build_char_operator_token_map() -> AHashMap<char, Tokens> {
        AHashMap::from([
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
            ('=', Tokens::Assignment),
        ])
    }

    pub fn build_char_punctuator_token_map() -> AHashMap<char, Tokens> {
        AHashMap::from([
            ('.', Tokens::Dot),
            ('{', Tokens::OpenCurlyBrace),
            ('}', Tokens::CloseCurlyBrace),
            ('(', Tokens::OpenParenthesis),
            (')', Tokens::CloseParenthesis),
            ('[', Tokens::OpenBrace),
            (']', Tokens::CloseBrace),
            (';', Tokens::Semicolon),
            (',', Tokens::Comma),
            (':', Tokens::Colon),
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

    // pub fn load(&'a mut self, source: String) {
    //     self.source = source;
    //     self.source_iterator = self.source.char_indices();
    // }

    // pub fn scan_old(&self, source: String) -> Result<ScannerResult, ScanError> {
    //     let mut start = 0;
    //     let mut line = 1;

    //     /* When we encounter a single-char lexeme like =, we keep proceeding
    //     until we encounter a char that is not a single-char lexeme */
    //     // Machine state defaults to Idle
    //     let mut scanner_state = ScannerState::Idle;

    //     let mut token_vec: Vec<Token> = Vec::with_capacity(size_of::<Token>() * 500);
    //     let mut source_iterator = source.char_indices().peekable();

    //     while let Some((i, char)) = source_iterator.next() {
    //         // thread::sleep(time::Duration::from_millis(1200));
    //         match char {
    //             ' ' => {
    //                 // Whitespace can  terminate a lexeme, be contiguous, or also be part of a string or comment.
    //                 // When first encountering white-space we will check for a lexeme between the start and current range
    //                 // `scanner_state` will then be set to `ScannerState::InWhitespace`
    //                 // If `scanner_state` is already `ScannerState::InWhitespace`, we will only increment start until non-white space is encountered

    //                 // We could be traversing white space in a comment or string literal
    //                 // Increment current and continue if so

    //                 match scanner_state {
    //                     ScannerState::InComment
    //                     | ScannerState::InBlockComment
    //                     | ScannerState::InStringDouble
    //                     | ScannerState::InStringSingle
    //                     | ScannerState::InStringTemplate => continue,
    //                     ScannerState::InWhitespace => {
    //                         start = i;
    //                         continue;
    //                     }
    //                     ScannerState::InNumber => {
    //                         // Exiting a number
    //                         // Can't validate number or get it's type without reaching a whitespace, newline, or operator
    //                         if let Some(token) = self.get_number_token(&source, start..i, line) {
    //                             start = i;
    //                             scanner_state = ScannerState::Idle;
    //                             token_vec.push(token);
    //                             continue;
    //                         }
    //                     }
    //                     _ => {
    //                         // When passing through newlines, start index gets incremented "onto" the new line
    //                         // Because of this, when we check for a token and get none, \n get's added as an "identifier"
    //                         // So we check for an imposter \n char
    //                         if &source[start..i] == "\n" {
    //                             start = i;
    //                             scanner_state = ScannerState::InWhitespace;
    //                             continue;
    //                         }
    //                         // We have encountered white space
    //                         // Check to see if lexeme between `start` and `i`
    //                         token_vec.push(self.get_token(&source, start..i, line).unwrap_or_else(
    //                             || {
    //                                 Token::new(
    //                                     line,
    //                                     &Tokens::Identifier,
    //                                     Some(source[start..i].to_string()),
    //                                 )
    //                             },
    //                         ));
    //                         // Since we are in white space, set this to true in case of contiguous
    //                         scanner_state = ScannerState::InWhitespace;
    //                     }
    //                 }

    //                 start = i;
    //                 continue;
    //             }
    //             '/' => {
    //                 if i == 0 {
    //                     continue;
    //                 }
    //                 // Could be a division token or the start/end of a comment/block comment
    //                 // Check if last char was another `/`
    //                 if &source[i - 1..i] == "/" {
    //                     // We are in a comment, change machine state
    //                     scanner_state = ScannerState::InComment;
    //                 } else if &source[i - 1..i] == "*" {
    //                     // We have exited a block comment
    //                     scanner_state = ScannerState::Idle;
    //                 } else {
    //                     // If we aren't in a comment, proceed as normal operator and catch in another match arm
    //                     scanner_state = ScannerState::InOperator;
    //                 }
    //             }
    //             '\n' | '\r' => {
    //                 // Although it's uncommon, statements can be multiline
    //                 // Newlines do delimit tokens
    //                 // Newlines terminate single-line comments

    //                 match scanner_state {
    //                     ScannerState::InStringDouble | ScannerState::InStringSingle => {
    //                         // Strings are't multiline outside of template strings
    //                         // Throw error here
    //                         return Err(ScanError::InvalidSyntax {
    //                             token: String::from("/\n"),
    //                             line,
    //                         });
    //                     }
    //                     ScannerState::InComment => {
    //                         // We were in a comment but have reached it's end, change to `Idle` state
    //                         scanner_state = ScannerState::Idle;
    //                     }
    //                     ScannerState::InBlockComment | ScannerState::Idle => {
    //                         start = i;
    //                         line += 1;
    //                         continue;
    //                     }
    //                     ScannerState::InPunctuator => {
    //                         // Most likely a semicolon
    //                         if let Some(token) = self.get_token(&source, start..i, line) {
    //                             token_vec.push(token);
    //                         }
    //                     }
    //                     _ => {
    //                         // Because newline delimits tokens, capture and add token to result vec
    //                         if let Some(token) = self.get_token(&source, start..(i - 1), line) {
    //                             token_vec.push(token);
    //                         }
    //                     }
    //                 }

    //                 // Scanner state is always idle when moving through newlines
    //                 scanner_state = ScannerState::Idle;
    //                 start = i;
    //                 line += 1;
    //                 continue;
    //             }
    //             '*' => {
    //                 // Asterisk can be an operator, part of an operator or signify the start of a block comment (if preceded by a `/`)
    //                 if scanner_state == ScannerState::InOperator {
    //                     // If we find a preceding '/' char, we're in a block comment
    //                     if &source[i - 1..i] == "/" {
    //                         scanner_state = ScannerState::InBlockComment;
    //                         continue;
    //                     }
    //                 }
    //                 if &source[i - 1..i] == "/" {
    //                     scanner_state = ScannerState::InBlockComment;
    //                 } else {
    //                     // If no '/' char is behind us, we must be part of a normal operator. Will catch in another match arm
    //                     scanner_state = ScannerState::InOperator;
    //                     start = i;
    //                     continue;
    //                 }
    //             }
    //             '`' => {
    //                 // TODO: Call `scan_template_string` here
    //             }
    //             _ => {
    //                 // Catch-all for the rest of the lexemes
    //                 match scanner_state {
    //                     ScannerState::InComment | ScannerState::InBlockComment => {
    //                         continue;
    //                     }
    //                     ScannerState::InStringSingle | ScannerState::InStringDouble => {
    //                         // Check to see if we're at the end of the string
    //                         // String ends with same char as it began, either ' or "
    //                         // Also check for escape backslash
    //                         if (char == '"' || char == '\'')
    //                             && (scanner_state == ScannerState::InStringDouble
    //                                 || scanner_state == ScannerState::InStringSingle)
    //                         {
    //                             // Check for escape at previous index
    //                             // Only check if the escaped char is the same as what started the string
    //                             if (char == '"' && scanner_state == ScannerState::InStringDouble
    //                                 || char == '\''
    //                                     && scanner_state == ScannerState::InStringSingle)
    //                                 && &source[i - 1..i] == "\\"
    //                             {
    //                                 // We are still in the string, continue
    //                                 continue;
    //                             }
    //                             token_vec.push(Token::new(
    //                                 line,
    //                                 &Tokens::String,
    //                                 Some(source[start..i + 1].to_string()),
    //                             ));
    //                             scanner_state = ScannerState::Idle;
    //                             start = i;
    //                             continue;
    //                         }
    //                     }
    //                     _ => {
    //                         // Is current char a lexeme
    //                         if let Some(char_token) = self.char_operator_token_map.get(&char) {
    //                             match scanner_state {
    //                                 ScannerState::InNumber => {
    //                                     // Number has been terminated
    //                                     if let Some(token) =
    //                                         Lexer::get_number_token(&self, &source, start..i, line)
    //                                     {
    //                                         token_vec.push(token);
    //                                         // scanner_state = ScannerState::InOperator;
    //                                         // start = i;
    //                                         // continue;
    //                                     }
    //                                 }
    //                                 ScannerState::InIdentifier => {
    //                                     // If we were in an identifier or other token, we need to close that out
    //                                     if let Some(token) = self.get_token(&source, start..i, line)
    //                                     {
    //                                         token_vec.push(token);
    //                                     } else {
    //                                         token_vec.push(Token::new(
    //                                             line,
    //                                             &Tokens::Identifier,
    //                                             Some(source[start..i].to_string()),
    //                                         ));
    //                                     }
    //                                 }
    //                                 ScannerState::InOperator => {
    //                                     // Current char is lexeme, and so was last
    //                                     continue;
    //                                 }
    //                                 ScannerState::InPunctuator => {
    //                                     // Last char was a punctuator, add to token_vec
    //                                     token_vec.push(Token::new(
    //                                         line,
    //                                         self.char_operator_token_map
    //                                             .get(&source[start..i].chars().next().unwrap())
    //                                             .unwrap(),
    //                                         None,
    //                                     ))
    //                                 }
    //                                 _ => (),
    //                             }
    //                             if Lexer::is_punctuator(char_token) == true {
    //                                 // token_vec.push(Token::new(line, char_token.to_owned(), None));
    //                                 scanner_state = ScannerState::InPunctuator;
    //                             } else {
    //                                 // If there was an identifier, it has been closed out, current char may be part of operator
    //                                 scanner_state = ScannerState::InOperator;
    //                             }
    //                             start = i;
    //                             continue;
    //                         } else {
    //                             match scanner_state {
    //                                 ScannerState::InNumber => {
    //                                     // We're in a number but the current char is not a number
    //                                     // It may be the end of the literal, check
    //                                     if Lexer::is_number_terminated(&source, i) == false {
    //                                         // If not yet terminated, we want to continue.
    //                                         // Else, we will transition below
    //                                         continue;
    //                                     }
    //                                 }
    //                                 ScannerState::InPunctuator => {
    //                                     // We know current char is not an operator or part of one
    //                                     // Last char (at index start) was a punctuator
    //                                     // We can assume that we are currently in an identifier
    //                                     if let Some(token_type) =
    //                                         self.char_operator_token_map.get(&char)
    //                                     {
    //                                         token_vec.push(Token::new(line, token_type, None));
    //                                     }
    //                                 }
    //                                 ScannerState::InIdentifier => {
    //                                     continue;
    //                                 }
    //                                 _ => (),
    //                             }
    //                             // White space is caught above. The char is not a lexeme itself (not an operator)
    //                             // At this point, we can't know if we're in a keyword or identifier. Must test
    //                             // Just exited an operator or literal, check for token between range
    //                             // Check for number
    //                             if let Ok(_) = char.to_string().parse::<i32>() {
    //                                 // all number types in JS at least start with a number
    //                                 if scanner_state != ScannerState::InNumber {
    //                                     scanner_state = ScannerState::InNumber;
    //                                     start = i;
    //                                 }
    //                                 continue;
    //                             }

    //                             if char == '"' && scanner_state != ScannerState::InStringDouble {
    //                                 scanner_state = ScannerState::InStringDouble;
    //                                 start = i;
    //                                 continue;
    //                             } else if char == '\''
    //                                 && scanner_state != ScannerState::InStringSingle
    //                             {
    //                                 scanner_state = ScannerState::InStringSingle;
    //                                 start = i;
    //                                 continue;
    //                             }
    //                             // if let Some(token) = self.get_token(&source_chars_chars, start..i, line) {
    //                             //     // There's a token, add to vec
    //                             //     token_vec.push(token);
    //                             // }
    //                             scanner_state = ScannerState::InIdentifier;
    //                             start = i;
    //                             continue;
    //                         }
    //                     }
    //                 }
    //             }
    //         }
    //         // println!("Scanner state is: {:#?}\n on char: {}", scanner_state, char);
    //     }
    //     Ok(ScannerResult {
    //         file_name: String::from("Hello World!"),
    //         token_vec
    //     })
    // }

    pub fn scan(&self, source: &'a String) -> Result<ScannerResult<'a>, ScanError> {
        let mut source_iterator = source.char_indices().peekable();
        let mut token_vec: Vec<Token> = Vec::with_capacity(size_of::<Token>() * 500);
        let mut line = 1;
        let mut index: usize;
        let mut current_char: char;
        static TOKEN_TYPE_ARRAY: [Tokens; 10] = [
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

        while let Some((_index, _current_char)) = source_iterator.next() {
            index = _index;
            current_char = _current_char;
            match current_char {
                ' ' => (),
                '\n' | '\r' => {
                    line += 1;
                }
                _ => {}
            }

            loop {
                if let Some((_, _char)) = source_iterator.peek() {
                    (index, current_char) = match _char {
                        '\n' | '\r' => break,
                        '"' | '\'' => {
                            if let Some((_index, _current_char)) =
                                self.scan_string(&mut source_iterator)
                            {
                                // println!("{}", &source[index + 1..=_index]);
                                token_vec.push(Token::new(
                                    line,
                                    &Tokens::String,
                                    Some(&source[index + 1..=_index]),
                                ));
                                (_index, _current_char)
                            } else {
                                (index, current_char)
                            }
                        }
                        '0'..='9' => {
                            if let Some((_index, number_type, _current_char)) =
                                self.scan_number(&mut source_iterator)
                            {
                                // println!(
                                //     "{:?} with value: {} on line: {}",
                                //     number_type,
                                //     &source[index + 1..=_index],
                                //     line
                                // );
                                token_vec.push(Token::new(
                                    line,
                                    &TOKEN_TYPE_ARRAY[number_type as usize],
                                    Some(&source[index + 1..=_index]),
                                ));
                                (_index, _current_char)
                            } else {
                                (index, current_char)
                            }
                        }
                        '.' if current_char == ' ' => {
                            if let Some((_index, number_type, _current_char)) =
                                self.scan_number(&mut source_iterator)
                            {
                                // println!(
                                //     "{:?} with value: {} on line: {}",
                                //     number_type,
                                //     &source[index + 1..=_index],
                                //     line
                                // );
                                token_vec.push(Token::new(
                                    line,
                                    &TOKEN_TYPE_ARRAY[number_type as usize],
                                    Some(&source[index + 1..=_index]),
                                ));
                                (_index, _current_char)
                            } else {
                                (index, current_char)
                            }
                        }
                        '*' | '/' if current_char == '/' => {
                            if let Some(_lines) = self.scan_comments(&mut source_iterator) {
                                line += _lines;
                            }
                            (index, current_char)
                        }
                        '/' if current_char == ' ' => break,
                        ' ' if current_char == '/' => {
                            // println!("/ on line: {}", line);
                            token_vec.push(Token::new(line, &Tokens::Divide, None));
                            // (index, current_char)
                            break;
                        }
                        ' ' => break,
                        'a'..='z' | 'A'..='Z' => {
                            if let Some((_index, _current_char)) =
                                self.scan_identifier(&mut source_iterator)
                            {
                                // println!("{:#?} on line: {}", &source[index + 1..=_index], line);
                                if let Some(token) =
                                    self.get_token(&source[index + 1..=_index], line)
                                {
                                    token_vec.push(token);
                                } else {
                                    token_vec.push(Token::new(
                                        line,
                                        &Tokens::Identifier,
                                        Some(&source[index + 1..=_index]),
                                    ));
                                }
                                (_index, _current_char)
                            } else {
                                (index, current_char)
                            }
                        }
                        _ => {
                            if let Some((_index, _current_char)) =
                                self.scan_operator(&mut source_iterator)
                            {
                                // println!("{:#?} on line: {}", &source[index + 1..=_index], line);
                                if let Some(token) =
                                    self.get_token(&source[index + 1..=_index], line)
                                {
                                    token_vec.push(token);
                                }
                                (_index, _current_char)
                            } else {
                                (index, current_char)
                            }
                        }
                    }
                } else {
                    break;
                }
            }
        }

        Ok(ScannerResult {
            file_name: String::from("Hello world!"),
            source,
            token_vec,
        })
    }

    fn scan_identifier(
        &self,
        source_iterator: &mut Peekable<CharIndices<'_>>,
    ) -> Option<(usize, char)> {
        // The item at `next` should have already been peeked, we're sure to be at the start of an identifier
        while let Some((index, current_char)) = source_iterator.next() {
            /* Things that terminate identifiers:
               punctuators,
               spaces,
               operators,
               newlines
            */
            // Chars form an identifier as long as we don't encounter a space, punctuator or operator
            if let Some((_, _char)) = source_iterator.peek() {
                match _char {
                    '\n' | '\r' | ' ' => return Some((index, current_char)),
                    ';' => return Some((index, current_char)),
                    _ => (),
                }
                if let Some(_) = self.char_operator_token_map.get(_char) {
                    // End of the lexeme!
                    return Some((index, current_char));
                } else if let Some(_) = self.char_punctuator_token_map.get(_char) {
                    return Some((index, current_char));
                }
            }
        }
        None
    }

    fn scan_number(
        &self,
        source_iterator: &mut Peekable<CharIndices<'_>>,
    ) -> Option<(usize, NumberType, char)> {
        let mut number_type = NumberType::Undetermined;
        while let Some((index, current_char)) = source_iterator.next() {
            /* Things that terminate numbers:
               spaces,
               most operators
               punctuators
               'n'
            */
            if let Some((_, _char)) = source_iterator.peek() {
                match _char {
                    // Single int
                    ' ' => return Some((index, NumberType::Int, current_char)),
                    '0'..='9' => {
                        if number_type != NumberType::Int && number_type != NumberType::Undetermined
                        {
                            continue;
                        }
                        number_type = NumberType::Int
                    }
                    '.' => number_type = NumberType::Float,
                    'x' | 'X' => number_type = NumberType::Hex,
                    'b' | 'B' => number_type = NumberType::Binary,
                    'o' | 'O' => number_type = NumberType::Octal,
                    'e' | 'E' => number_type = NumberType::Exponential,
                    'A'..='F' | 'a'..='f' => {
                        if number_type != NumberType::Hex {
                            return None;
                        }
                    }
                    '+' | '-' => {
                        if number_type != NumberType::Exponential {
                            return Some((index, number_type, current_char));
                        }
                    }
                    'n' => {
                        // Move past 'n'
                        source_iterator.next();
                        match number_type {
                            NumberType::Int => {
                                return Some((index + 1, NumberType::BigInt, current_char))
                            }
                            NumberType::Hex => {
                                return Some((index + 1, NumberType::BigHex, current_char))
                            }
                            NumberType::Octal => {
                                return Some((index + 1, NumberType::BigOctal, current_char))
                            }
                            NumberType::Binary => {
                                return Some((index + 1, NumberType::BigBinary, current_char))
                            }
                            NumberType::BigInt => return None,
                            NumberType::BigHex => return None,
                            NumberType::BigBinary => return None,
                            NumberType::BigOctal => return None,
                            _ => (),
                        }
                    }

                    _ => {
                        if ('0'..='9').contains(&current_char)
                            && number_type == NumberType::Undetermined
                        {
                            return Some((index, NumberType::Int, current_char));
                        }
                        return Some((index, number_type, current_char));
                    }
                }
            }
        }
        None
    }

    fn scan_operator(
        &self,
        source_iterator: &mut Peekable<CharIndices<'_>>,
    ) -> Option<(usize, char)> {
        // We know current_char is a lexeme
        let mut is_potential_spread = false;
        while let Some((index, current_char)) = source_iterator.next() {
            if let Some((_, _char)) = source_iterator.peek() {
                // Char is at least a lexeme, check if it's a punctuator (most likely)
                match _char {
                    '\n' | '\r' => return Some((index, current_char)),
                    '.' => {
                        if !is_potential_spread {
                            is_potential_spread = true;
                        } else {
                            if *_char == '.' {
                                // Spread found, exit early
                                source_iterator.next();
                                return Some((index + 1, current_char));
                            }
                        }
                    }
                    _ => {
                        let is_operator = self.char_operator_token_map.contains_key(_char);
                        if !is_operator {
                            return Some((index, current_char));
                        }
                    }
                }
            }
        }
        None
    }

    fn scan_string(
        &self,
        source_iterator: &mut Peekable<CharIndices<'_>>,
    ) -> Option<(usize, char)> {
        let mut is_double = false;
        let mut is_started = false;
        while let Some((index, current_char)) = source_iterator.next() {
            // What type of string are we?
            match current_char {
                '"' if !is_double && !is_started => {
                    is_double = true;
                    is_started = true;
                }
                '"' if is_double && is_started => return Some((index, current_char)),
                '\'' if !is_double && !is_started => {
                    is_started = true;
                }
                '\'' if !is_double && is_started => return Some((index, current_char)),
                '\\' => {
                    // Move past escaped chars!
                    source_iterator.next();
                }
                _ => (),
            }
        }
        None
    }

    fn scan_comments(&self, source_iterator: &mut Peekable<CharIndices<'_>>) -> Option<u16> {
        let mut num_newlines = 0;
        let is_block_comment = source_iterator.peek().unwrap().1 == '*';
        while let Some((index, current_char)) = source_iterator.next() {
            if let Some((_, _char)) = source_iterator.peek() {
                match (current_char, _char) {
                    (_, '\n') if !is_block_comment => return Some(num_newlines),
                    (_, '\n') => num_newlines += 1,
                    ('*', '/') => return Some(num_newlines),
                    _ => (),
                }
            }
        }
        None
    }

    fn scan_template_string(source: &Vec<char>) {
        // TODO: Implement
        unimplemented!();
    }

    fn get_token(&self, lexeme: &str, line: u16) -> Option<Token<'a>> {
        // Check `token_map` to see if string is a keyword or operator
        if lexeme.len() > 1 {
            if let Some(token) = self.token_map.get(lexeme) {
                // We have found a keyword/operator, now add it to the result vector
                // keywords/operators do not need to have their literal copied into the token
                Some(Token::new(line, token, None))
            } else {
                None
            }
        } else {
            // Check for single-char lexeme
            let char = lexeme.chars().next().unwrap_or_default();
            if let Some(token) = self.char_operator_token_map.get(&char) {
                // keywords/operators do not need to have their literal copied into the token
                Some(Token::new(line, token, None))
            } else {
                None
            }
        }
    }

    // fn get_number_type(&self, source: &str, range: &Range<usize>) -> Option<NumberType> {
    //     // Get string of the first couple chars
    //     let literal = match source.get(range.start..range.end) {
    //         Some(result) => result,
    //         None => {
    //             return None;
    //         }
    //     };
    //     // TODO: Convert this slop to tuples
    //     if let Some(slice) = source.get(0..2) {
    //         match slice {
    //             "0x" | "0X" => {
    //                 // Hex
    //                 if literal.ends_with('n') == true {
    //                     // BigHex
    //                     return Some(NumberType::BigHex);
    //                 }
    //                 return Some(NumberType::Hex);
    //             }
    //             "0b" | "0B" => {
    //                 // Binary
    //                 if literal.ends_with('n') == true {
    //                     // BigBinary
    //                     return Some(NumberType::BigBinary);
    //                 }
    //                 return Some(NumberType::Binary);
    //             }
    //             "0o" | "0O" => {
    //                 // Octal
    //                 if literal.ends_with('n') == true {
    //                     return Some(NumberType::BigOctal);
    //                 }
    //                 return Some(NumberType::Octal);
    //             }
    //             _ => {
    //                 // Check if number is exponential, if it is, return
    //                 if Lexer::is_number_exponential(source, range.start..range.end) == true {
    //                     return Some(NumberType::Exponential);
    //                 }
    //                 // Exponential have n digits followed by 'e' followed by an optional +- and then digits
    //                 // BigInt only ends with 'n', cannot start with 0
    //                 if literal.ends_with('n') == true {
    //                     return Some(NumberType::BigInt);
    //                 } else {
    //                     // Normal int or decimal
    //                     for char in literal.chars() {
    //                         if char == '.' {
    //                             return Some(NumberType::Float);
    //                         }
    //                     }
    //                     return Some(NumberType::Int);
    //                 }
    //             }
    //         }
    //     }
    //     None
    // }

    // fn get_number_token(&self, source: &str, range: Range<usize>, line: u16) -> Option<Token> {
    //     // Validate number and get it's type
    //     // Array is on the stack, BLAZINGLY fast
    //     static token_type_array: [Tokens; 10] = [
    //         Tokens::Float,
    //         Tokens::Int,
    //         Tokens::Hex,
    //         Tokens::Octal,
    //         Tokens::Binary,
    //         Tokens::Exponential,
    //         Tokens::BigInt,
    //         Tokens::BigHex,
    //         Tokens::BigBinary,
    //         Tokens::BigOctal,
    //     ];

    //     if let Some(number_type) = self.get_number_type(&source, &range) {
    //         let literal = &source[range.start..range.end];
    //         return Some(Token::new(
    //             line,
    //             &token_type_array[number_type as usize],
    //             Some(literal.to_owned()),
    //         ));
    //     }
    //     None
    // }

    // fn is_number_exponential(source: &str, range: Range<usize>) -> bool {
    //     let source_chars = &source[range];
    //     if source_chars.len() == 0 {
    //         return false;
    //     }
    //     let mut e_found = false;
    //     let mut sign_found = false;
    //     for char in source_chars.chars() {
    //         // If we have already encountered 'e', the rest must be numbers or +/-
    //         // If not, continue to parse numbers.
    //         if let Ok(_) = char.to_string().parse::<i32>() {
    //         } else if char == '+' || char == '-' {
    //             if sign_found == true {
    //                 // Invalid, return false
    //                 return false;
    //             }
    //             sign_found = true;
    //         } else if char == 'e' || char == 'E' {
    //             if e_found == true {
    //                 // Invalid, return false
    //                 return false;
    //             }
    //             e_found = true;
    //         } else {
    //             return false;
    //         }
    //     }
    //     return e_found;
    // }

    // fn is_number_terminated(source: &str, index: usize) -> bool {
    //     // To be used when an operator has been encountered while scanning through a number
    //     // Numbers are terminated by spaces, operators, or chars not acceptable for their type
    //     // We can only validate a number after it's boundaries are found.
    //     // This function tests if we are at the end boundary
    //     match (&source[index - 1..index], &source[index..index]) {
    //         ("e" | "E", "+" | "-") => false,
    //         _ => true,
    //     }
    // }

    // fn is_punctuator(token: &Tokens) -> bool {
    //     match token {
    //         Tokens::OpenCurlyBrace
    //         | Tokens::CloseCurlyBrace
    //         | Tokens::OpenParenthesis
    //         | Tokens::CloseParenthesis
    //         | Tokens::OpenBrace
    //         | Tokens::CloseBrace
    //         | Tokens::Semicolon
    //         | Tokens::Comma
    //         | Tokens::Dot
    //         | Tokens::Colon => true,
    //         _ => false,
    //     }
    // }

    pub fn get_maps() -> (
        AHashMap<&'static str, Tokens>,
        AHashMap<char, Tokens>,
        AHashMap<char, Tokens>,
    ) {
        (
            Lexer::build_token_map(),
            Lexer::build_char_operator_token_map(),
            Lexer::build_char_punctuator_token_map(),
        )
    }

    pub fn new(
        token_map: &'a AHashMap<&'static str, Tokens>,
        char_operator_token_map: &'a AHashMap<char, Tokens>,
        char_punctuator_token_map: &'a AHashMap<char, Tokens>,
    ) -> Lexer<'a> {
        Lexer {
            token_map,
            char_operator_token_map,
            char_punctuator_token_map,
        }
    }
}
