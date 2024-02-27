use crate::token::{Kind, Token};
use colored::Colorize;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum SyntaxError {
    #[error("lexer error: {0}")]
    LexerError(String),
    #[error("parser error: {0}")]
    ParserError(String),
    #[error("runtime error: {0}")]
    RuntimeError(String),
}

pub struct Reporter {
    pub had_error: bool,
}

impl Reporter {
    pub fn new() -> Self {
        Self { had_error: false }
    }

    pub fn error(&mut self, line: usize, message: &str) {
        self.report(line, "", message);
    }

    pub fn report(&mut self, line: usize, at: &str, message: &str) {
        println!(
            "Error: [line {}] Error {}: {}",
            line,
            at,
            message.bright_red()
        );
        self.had_error = true;
    }

    pub fn error_token(&mut self, token: &Token, message: &str) {
        if token.kind == Kind::EOF {
            self.report(token.line, " at end", message);
        } else {
            self.report(token.line, &format!("at '{}'", token.lexeme), message);
        }
    }
}
