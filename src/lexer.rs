use crate::reporter::Reporter;
use crate::token::{map_keyword, Kind, Token};
use std::str::Chars;

pub struct Lexer<'a> {
    /// Source Text
    source: &'a str,

    /// The remaining characters
    chars: Chars<'a>,

    start: usize,
    current: usize,

    /// The current line
    line: usize,
    pub reporter: Reporter,

    pub tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str, reporter: Reporter) -> Self {
        Self {
            source,
            chars: source.chars(),
            start: 0,
            current: 0,
            line: 1,
            reporter: reporter,
            tokens: Vec::new(),
        }
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
                    self.reporter
                        .error(self.line, "lexer: unexpected character")
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

    fn identifier(&mut self) {
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.advance();
        }

        let text = &self.source[self.start..self.current];
        self.add_token(map_keyword(text));
    }

    fn string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            self.reporter.error(self.line, "Unterminated string.");
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
            line: self.line,
            lexeme: String::from(&self.source[self.start..self.current]),
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
