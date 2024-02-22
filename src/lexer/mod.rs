#![allow(non_camel_case_types)]

use std::str::Chars;
use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token {
    pub kind: Kind,
    pub start: usize,
    pub end: usize,
    pub line: usize,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Kind {
    // Single-character tokens.
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    // Literals.
    IDENTIFIER,
    STRING,
    NUMBER,

    // Keywords.
    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    EOF,
}

pub struct Lexer<'a> {
    /// Source Text
    source: &'a str,

    /// The remaining characters
    chars: Chars<'a>,

    start: usize,
    current: usize,

    /// The current line
    line: usize,
    pub had_error: bool,

    tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.chars(),
            start: 0,
            current: 0,
            line: 1,
            had_error: false,
            tokens: Vec::new(),
        }
    }

    fn error(&mut self, line: usize, message: &str) {
        self.report(line, "", message);
    }

    fn report(&mut self, line: usize, at: &str, message: &str) {
        println!("Error: [line {}] Error {}: {}", line, at, message);
        self.had_error = true;
    }

    pub fn scan_tokens(&mut self) -> Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }
        self.add_token(Kind::EOF);
        self.tokens.clone()
    }

    fn scan_token(&mut self) {
        let c = self.advance();
        match c {
            '(' => self.add_token(Kind::LEFT_PAREN),
            ')' => self.add_token(Kind::RIGHT_PAREN),
            '{' => self.add_token(Kind::LEFT_BRACE),
            '}' => self.add_token(Kind::RIGHT_BRACE),
            ',' => self.add_token(Kind::COMMA),
            '.' => self.add_token(Kind::DOT),
            '-' => self.add_token(Kind::MINUS),
            '+' => self.add_token(Kind::PLUS),
            ';' => self.add_token(Kind::SEMICOLON),
            '*' => self.add_token(Kind::STAR),
            '!' => {
                if self.consume_if('=') {
                    self.add_token(Kind::BANG_EQUAL);
                } else {
                    self.add_token(Kind::BANG);
                }
            }
            '=' => {
                if self.consume_if('=') {
                    self.add_token(Kind::EQUAL_EQUAL);
                } else {
                    self.add_token(Kind::EQUAL);
                }
            }
            '<' => {
                if self.consume_if('=') {
                    self.add_token(Kind::LESS_EQUAL);
                } else {
                    self.add_token(Kind::LESS);
                }
            }
            '>' => {
                if self.consume_if('=') {
                    self.add_token(Kind::GREATER_EQUAL);
                } else {
                    self.add_token(Kind::GREATER);
                }
            }
            '/' => {
                if self.consume_if('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(Kind::SLASH);
                }
            }
            ' ' | '\r' | '\t' => {}
            '\n' => {
                self.line += 1;
            }
            '"' => self.string(),
            _ => {
                if c.is_digit(10) {
                    self.number();
                } else if c.is_alphabetic() || c == '_' {
                    self.identifier();
                } else {
                    self.error(self.line, "unexpected character")
                }
            }
        }
    }

    fn number(&mut self) {
        while self.peek().is_digit(10) {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_digit(10) {
            self.advance();
            while self.peek().is_digit(10) {
                self.advance();
            }
        }

        self.add_token(Kind::NUMBER);
    }

    fn map_keyword(&self, text: &str) -> Kind {
        match text {
            "and" => Kind::AND,
            "class" => Kind::CLASS,
            "else" => Kind::ELSE,
            "false" => Kind::FALSE,
            "for" => Kind::FOR,
            "fun" => Kind::FUN,
            "if" => Kind::IF,
            "nil" => Kind::NIL,
            "or" => Kind::OR,
            "print" => Kind::PRINT,
            "return" => Kind::RETURN,
            "super" => Kind::SUPER,
            "this" => Kind::THIS,
            "true" => Kind::TRUE,
            "var" => Kind::VAR,
            "while" => Kind::WHILE,
            _ => Kind::IDENTIFIER,
        }
    }

    fn identifier(&mut self) {
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.advance();
        }

        let text = &self.source[self.start..self.current];
        self.add_token(self.map_keyword(text));
    }

    fn string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            self.error(self.line, "Unterminated string.");
            return;
        }

        // The closing ".
        self.advance();

        self.add_token(Kind::STRING);
    }

    fn consume_if(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.peek() == expected {
            self.advance();
            return true;
        }
        false
    }

    fn add_token(&mut self, kind: Kind) {
        let token = Token {
            kind: kind,
            start: self.start,
            end: self.current,
            line: self.line,
        };
        self.tokens.push(token);
    }

    fn is_at_end(&self) -> bool {
        self.chars.clone().next().is_none()
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.chars.next().unwrap_or('\0')
    }

    fn peek(&self) -> char {
        self.chars.clone().next().unwrap_or('\0')
    }

    fn peek_next(&self) -> char {
        let mut chars = self.chars.clone();
        chars.next();
        chars.next().unwrap_or('\0')
    }
}

#[derive(Debug, Error)]
pub enum SyntaxError {
    #[error("Unexpected Token")]
    UnexpectedToken,

    #[error("Expected a semicolon or an implicit semicolon after a statement, but found none")]
    AutoSemicolonInsertion,

    #[error("Unterminated multi-line comment")]
    UnterminatedMultiLineComment,
}

pub type Result<T> = std::result::Result<T, SyntaxError>;
