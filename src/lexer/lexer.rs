use std::{iter::Peekable, mem::size_of, str::CharIndices};

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
                            // First check for a punctuator
                            if let Some((_index, _current_char)) =
                                self.scan_punctuator(&mut source_iterator)
                            {
                                if let Some(token) =
                                    self.get_token(&source[index + 1..=_index], line)
                                {
                                    token_vec.push(token);
                                }
                                (_index, _current_char)
                            } else {
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

    fn scan_punctuator(
        &self,
        source_iterator: &mut Peekable<CharIndices<'_>>,
    ) -> Option<(usize, char)> {
        // First, peek and check punctuator map for char
        // Peek because we don't want to consume the char in case it's an operator
        if let Some((_index, peeked_char)) = source_iterator.peek() {
            // Check the punctuator map for the peeked char
            if self.char_punctuator_token_map.contains_key(peeked_char) {
                // Advance iterator and return result
                return Some(source_iterator.next().unwrap());
            } else {
                return None;
            }
        }
        None
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
                // Check for punctuator
                if let Some(token) = self.char_punctuator_token_map.get(&char) {
                    Some(Token::new(line, token, None))
                } else {
                    None
                }
            }
        }
    }

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
