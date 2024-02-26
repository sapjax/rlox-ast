use crate::token::{Kind, Token};
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
        println!("Error: [line {}] Error {}: {}", line, at, message);
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
