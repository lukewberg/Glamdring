use std::{collections::HashMap, ops::Range};

use crate::types::{ScanError, ScannerState, Token, Tokens};

pub struct JsLexer<'a> {
    pub source: &'a str,
    token_map: HashMap<&'static str, Tokens>,
    char_token_map: HashMap<char, Tokens>,
}

impl<'a> JsLexer<'a> {
    fn build_token_map() -> HashMap<&'static str, Tokens> {
        let mut token_map = HashMap::from([
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

    pub fn scan(&self) -> Result<Vec<Token>, ScanError> {
        let mut start = 0;
        let mut current = 0;

        /* When we encounter a single-char lexeme like =, we keep proceeding
        until we encounter a char that is not a single-char lexeme */

        // Machine states default to Idle
        let mut scanner_state = ScannerState::Idle;

        // Convert source string into a char vec so we can index into it
        let source_chars: Vec<char> = self.source.chars().collect();

        let mut token_vec: Vec<Token> = Vec::new();

        for (i, char) in source_chars.iter().enumerate() {
            match char {
                ' ' => {
                    // Whitespace can  terminate a lexeme, be contiguous, or also be part of a string or comment.
                    // When first encountering white-space we will check for a lexeme between the start and current range
                    // `in_whitespace` will then be set to true
                    // If `in_whitespace` is already true, we will only increment start until non-white space is encountered

                    // We could be traversing white space in a comment or string literal
                    // Increment current and continue if so
                    if scanner_state == ScannerState::InComment
                        || scanner_state == ScannerState::InBlockComment
                        || scanner_state == ScannerState::InString
                    {
                        current = i;
                        continue;
                    } else if scanner_state == ScannerState::InWhitespace {
                        start = i;
                        continue;
                    } else {
                        // We have encountered white space
                        // Check to see if lexeme between `start` and `i`
                        self.get_token(start..i, |token| {
                            // If a token is found, add it to the `token_vec`
                            token_vec.push(token);
                        });
                        // Since we are in white space, set this to true in case of contiguous
                        scanner_state = ScannerState::InWhitespace;

                        start = i;
                    }
                }
                '/' => {
                    // Could be a division token or the start of a comment/block comment
                    // Check if last char was another `/`

                    if source_chars[i - 1] == '/' {
                        // We are in a comment, change machine state
                        scanner_state = ScannerState::InComment;
                    }
                }
                '\n' => {
                    // Although it's uncommon, statements can be multiline
                    // Newlines terminate single-line comments
                    if scanner_state == ScannerState::InComment {
                        // We were in a comment but have reached it's end, change to `Idle` state
                        scanner_state = ScannerState::Idle;
                    }
                }
                '*' => {
                    // Asterisk can be an operator or signify the end of a block comment
                    if source_chars[i+1] == '/' {
                        scanner_state = ScannerState::Idle;
                    }
                }
                _ => {
                    // Catch-all for the rest of the lexemes
                }
            }
        }
        Ok(token_vec)
    }

    fn peek(&self, target: char, index: usize) -> bool {
        if index < self.source.len() {
            self.source.chars().nth(index).unwrap() == target
        } else {
            false
        }
    }

    fn get_token<F: FnMut(Token)>(&self, range: Range<usize>, mut f: F) -> () {
        // Check `token_map` to see if string is a keyword or operator
        let lexeme = &self.source[range.to_owned()];
        if let Some(token) = self.token_map.get(&lexeme[..]) {
            // We have found a keyword/operator, now add it to the result vector
            f(Token::new(range, token.clone(), lexeme.to_string()));
        } else {
            // If it's neither, must be an identifier.
            f(Token::new(range, Tokens::Identifier, lexeme.to_string()));
        }
    }

    fn check_token(&self, range: Range<usize>) -> bool {
        self.token_map.contains_key(&self.source[range])
    }

    pub fn new(source: &str) -> JsLexer {
        JsLexer {
            source,
            token_map: Self::build_token_map(),
            char_token_map: Self::build_char_token_map(),
        }
    }
}
