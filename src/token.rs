#[derive(Clone, PartialEq)]
pub enum TokenKind {
    Ampersand,
    Asterisk,
    AtSign,
    Backslash,
    Backtick,
    Caret,
    Colon,
    Comma,
    Comment,
    DollarSign,
    DoubleQuote,
    EOF,
    Equal,
    ExclamationMark,
    GreaterThan,
    GreaterThanOrEqual,
    Hash,
    Hyphen,
    LeftCurlyBracket,
    LeftParenthesis,
    LeftSquareBracket,
    LessThan,
    LessThanOrEqual,
    Number,
    PercentSign,
    Period,
    PlusSign,
    QuestionMark,
    RightArrow,
    RightCurlyBracket,
    RightParenthesis,
    RightSquareBracket,
    SingleQuote,
    Slash,
    Tilde,
    Underscore,
    VerticalBar,
    Word,
}

#[derive(Clone, PartialEq)]
pub struct Token {
    pub line: usize,
    pub column: usize,
    pub text: String,
    pub kind: TokenKind,
}
