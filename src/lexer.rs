use crate::error::error;
use crate::token::{Token, TokenKind};

pub struct Lexer {
    chars: Vec<char>, // the file as a vector of chars
    index: usize,     // the lexer's position in the current file (= the index in the vector chars)
    line: usize,      // line number of the current position (zero-based)
    column: usize,    // column number of the current position (zero-based)
}

impl Lexer {
    pub fn new(file: String) -> Self {
        Lexer {
            chars: file.chars().collect(),
            index: 0,
            line: 0,
            column: 0,
        }
    }

    #[inline]
    fn peek(&self, offset: usize) -> Option<char> {
        match self.index.checked_add(offset) {
            Some(index) if index < self.chars.len() => Some(self.chars[index]),
            _ => None
        }
    }

    fn consume_whitespace(&mut self) {
        while let Some(char) = self.peek(0) {
            if !char.is_whitespace() {
                return;
            }

            if char == '\n' {
                self.line += 1;
                self.column = 0;
            } else {
                self.column += 1;
            }

            self.index += 1;
        }
    }

    fn consume_word(&mut self) -> Token {
        let original_index = self.index;
        let original_column = self.column;

        while let Some(char) = self.peek(0) {
            if !char.is_alphanumeric() && char != '_' {
                break;
            }

            self.column += 1;
            self.index += 1;
        }

        Token {
            line: self.line,
            column: original_column,
            text: self.chars[original_index..self.index].into_iter().collect(),
            kind: TokenKind::Word,
        }
    }

    fn consume_number(&mut self) -> Token {
        let original_index = self.index;
        let original_column = self.column;

        while let Some(char) = self.peek(0) {
            if char.is_alphabetic() {
                error("Invalid decimal number.", self.line, original_column);
            }

            if !char.is_digit(10) {
                break;
            }

            self.column += 1;
            self.index += 1;
        }

        Token {
            line: self.line,
            column: original_column,
            text: self.chars[original_index..self.index].into_iter().collect(),
            kind: TokenKind::Number,
        }
    }

    fn consume_comment(&mut self) -> Token {
        let original_line = self.line;
        let original_column = self.column;

        while let Some(char) = self.peek(0) {
            if char == '\n' {
                self.line += 1;
                self.column = 0;
                self.index += 1;
                break;
            } else {
                self.column += 1;
                self.index += 1;
            }
        }

        Token {
            line: original_line,
            column: original_column,
            text: String::new(),
            kind: TokenKind::Comment,
        }
    }

    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn consume_symbols(&mut self) -> Token {
        if self.peek(0) == Some(';') {
            return self.consume_comment();
        }
        
        let (kind, advance_by) = match (self.peek(0), self.peek(1)) {
            (Some('!') ,  _        ) => (TokenKind::ExclamationMark,    1),
            (Some('"') ,  _        ) => (TokenKind::DoubleQuote,        1),
            (Some('#') ,  _        ) => (TokenKind::Hash,               1),
            (Some('$') ,  _        ) => (TokenKind::DollarSign,         1),
            (Some('%') ,  _        ) => (TokenKind::PercentSign,        1),
            (Some('&') ,  _        ) => (TokenKind::Ampersand,          1),
            (Some('\''),  _        ) => (TokenKind::SingleQuote,        1),
            (Some('(') ,  _        ) => (TokenKind::LeftParenthesis,    1),
            (Some(')') ,  _        ) => (TokenKind::RightParenthesis,   1),
            (Some('*') ,  _        ) => (TokenKind::Asterisk,           1),
            (Some('+') ,  _        ) => (TokenKind::PlusSign,           1),
            (Some(',') ,  _        ) => (TokenKind::Comma,              1),
            (Some('-') ,  _        ) => (TokenKind::Hyphen,             1),
            (Some('.') ,  _        ) => (TokenKind::Period,             1),
            (Some('/') ,  _        ) => (TokenKind::Slash,              1),
            (Some(':') ,  _        ) => (TokenKind::Colon,              1),
            (Some('<') ,  Some('=')) => (TokenKind::LessThanOrEqual,    2),
            (Some('<') ,  _        ) => (TokenKind::LessThan,           1),
            (Some('=') ,  Some('>')) => (TokenKind::RightArrow,         2),
            (Some('=') ,  _        ) => (TokenKind::Equal,              1),
            (Some('>') ,  Some('=')) => (TokenKind::GreaterThanOrEqual, 2),
            (Some('>') ,  _        ) => (TokenKind::GreaterThan,        1),
            (Some('?') ,  _        ) => (TokenKind::QuestionMark,       1),
            (Some('@') ,  _        ) => (TokenKind::AtSign,             1),
            (Some('[') ,  _        ) => (TokenKind::LeftSquareBracket,  1),
            (Some('\\'),  _        ) => (TokenKind::Backslash,          1),
            (Some(']') ,  _        ) => (TokenKind::RightSquareBracket, 1),
            (Some('^') ,  _        ) => (TokenKind::Caret,              1),
            (Some('_') ,  _        ) => (TokenKind::Underscore,         1),
            (Some('`') ,  _        ) => (TokenKind::Backtick,           1),
            (Some('{') ,  _        ) => (TokenKind::LeftCurlyBracket,   1),
            (Some('|') ,  _        ) => (TokenKind::VerticalBar,        1),
            (Some('}') ,  _        ) => (TokenKind::RightCurlyBracket,  1),
            (Some('~') ,  _        ) => (TokenKind::Tilde,              1),
            _ => unreachable!("The above should cover all possible cases."),
        };

        let original_column = self.column;

        self.column += advance_by;
        self.index += advance_by;

        Token {
            line: self.line,
            column: original_column,
            text: String::new(),
            kind,
        }
    }

    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn consume_token(&mut self) -> Token {
        self.consume_whitespace();

        if let Some(char) = self.peek(0) {
            match char {
                _ if char.is_alphabetic() || char == '_' => self.consume_word(),
                _ if char.is_digit(10)                   => self.consume_number(),
                _ if char.is_ascii_punctuation()         => self.consume_symbols(),
                _                                        => {
                    error("Unexpected character.", self.line, self.column);
                    unreachable!("The above function terminates the program.");
                }
            }
        } else {
            Token {
                line: self.line,
                column: self.column,
                text: String::new(),
                kind: TokenKind::EOF
            }
        }
    }

    pub fn tokens(self) -> impl Iterator<Item = Token> {
        LexerIterator::new(self)
    }
}

struct LexerIterator {
    lexer: Lexer,
    next_token: Option<Token>,
}

impl LexerIterator {
    fn new(mut lexer: Lexer) -> Self {
        // at this point, there's at least one next token; even
        // if the file is empty, the next token will be EOF
        let next_token = Some(lexer.consume_token());

        LexerIterator { lexer, next_token }
    }
}

impl Iterator for LexerIterator {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match &self.next_token {
            None => None,
            Some(token) => {
                let current = self.next_token.clone();
                self.next_token = if token.kind == TokenKind::EOF {
                    None
                } else {
                    Some(self.lexer.consume_token())
                };

                current
            }
        }
    }
}
