use std::{collections::HashMap, ops::Range};
#[derive(Debug, Clone)]
pub enum Tokens {
    Await,
    Break,
    Case,
    Catch,
    Class,
    Const,
    Continue,
    Debugger,
    Default,
    Delete,
    Do,
    Else,
    Enum,
    Export,
    Extends,
    False,
    Finally,
    For,
    Function,
    If,
    Import,
    In,
    Instanceof,
    New,
    Null,
    Return,
    Super,
    Switch,
    This,
    Throw,
    True,
    Try,
    Typeof,
    Var,
    Void,
    While,
    With,
    Yield,
    OptionalChain,
    OpenCurlyBrace,
    CloseCurlyBrace,
    OpenParenthesis,
    CloseParenthesis,
    OpenBrace,
    CloseBrace,
    Dot,
    Ellipsis,
    Semicolon,
    Comma,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    Equality,
    NotEqual,
    StrictEqual,
    NotStrictEqual,
    UnaryPlus,
    UnaryNegation,
    Multiply,
    Divide,
    Modulo,
    Exponentiation,
    Increment,
    Decrement,
    LeftShift,
    RightShift,
    UnsignedRightShift,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    Bang,
    BitwiseNot,
    And,
    Or,
    NullishCoalescing,
    Ternary,
    Colon,
    Assignment,
    AdditionAssignment,
    SubtractionAssignment,
    MultiplicationAssignment,
    RemainderAssignment,
    ExponentiationAssignment,
    LeftShiftAssignment,
    RightShiftAssignment,
    UnsignedRightShiftAssignment,
    BitwiseAndAssignment,
    BitwiseOrAssignment,
    BitwiseXorAssignment,
    AndAssignment,
    OrAssignment,
    DivisionAssignment,
    NullishCoalescingAssignment,
    ArrowNotation,
    StringLiteral,
    NumericLiteral,
    Identifier,
}

#[derive(Debug)]
pub enum ScanError {
    UnexpectedCharacter { character: char, location: usize },
    UnexpectedToken { token: String, location: usize },
    UnexpectedOperator { token: String, location: usize },
    UnexpectedIdentifier { token: String, location: usize },
}

#[derive(PartialEq, Eq)]
pub enum ScannerState {
    Idle,
    InIdentifier,
    InOperator,
    InNumber,
    InString,
    InComment,
    InBlockComment,
    InWhitespace,
}

#[derive(Debug, Clone)]
pub struct Token {
    // pub start: u32,
    // pub end: u32,
    pub range: Range<usize>,
    pub token_type: Tokens,
    pub lexeme: String,
}

impl Token {
    pub fn new(range: Range<usize>, token_type: Tokens, lexeme: String) -> Token {
        Token {
            range,
            token_type,
            lexeme,
        }
    }
}

pub enum TestEnum {
    Heyy(String),
    Test(i32),
}
pub struct Test(HashMap<String, TestEnum>);

impl Test {
    pub fn new() -> Test {
        let mut book_reviews = HashMap::new();
        book_reviews.insert(String::from("Hello"), TestEnum::Test(5));

        Test(book_reviews)
    }
}
