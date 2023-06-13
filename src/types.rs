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
    Let,
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
    String,
    TemplateString,
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
    Int,
    Float,
    Hex,
    Octal,
    Binary,
    Exponential,
    Infinity,
    BigInt,
    BigFloat,
    BigHex,
    BigBinary,
    BigOctal,
}

#[derive(Debug)]
pub enum ScanError {
    UnexpectedCharacter { character: char, line: u16 },
    UnexpectedToken { token: String, line: u16 },
    UnexpectedOperator { token: String, line: u16 },
    UnexpectedIdentifier { token: String, line: u16 },
    InvalidSyntax { token: String, line: u16 },
}

#[derive(PartialEq, Eq, Debug)]
pub enum ScannerState {
    Idle,
    InIdentifier,
    InOperator,
    InNumber,
    InStringDouble,
    InStringSingle,
    InStringTemplate,
    InComment,
    InBlockComment,
    InWhitespace,
    InPunctuator,
}

#[derive(Debug)]
pub struct ScannerResult {
    pub file_name: String,
    pub token_vec: Vec<Token>,
}

#[derive(Debug, Clone)]
pub struct Token {
    // pub start: u32,
    // pub end: u32,
    pub line: u16,
    pub token_type: Tokens,
    pub lexeme: Option<String>,
    pub children: Option<Vec<Box<Token>>>,
}

impl Token {
    pub fn new(line: u16, token_type: Tokens, lexeme: Option<String>) -> Token {
        Token {
            line,
            token_type,
            lexeme,
            children: None,
        }
    }

    pub fn add_child(mut self, token: Token) -> () {
        if let Some(mut children) = self.children {
            children.push(Box::new(token));
        } else {
            self.children = Some(vec![Box::new(token)]);
        }
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum NumberType {
    Float,
    Int,
    Hex,
    Octal,
    Binary,
    Exponential,
    BigInt,
    BigHex,
    BigBinary,
    BigOctal,
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
