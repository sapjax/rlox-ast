/*

==================== Grammar ====================
expression     → equality ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary
               | primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" ;
==================== Grammar ====================

*/

use crate::ast::{
    BinaryExpression, Expr, GroupingExpression, LiteralExpression, Object, UnaryExpression,
};
use crate::reporter::{Reporter, SyntaxError};
use crate::token::{Kind, Token};

pub type Result<T> = std::result::Result<T, SyntaxError>;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    pub reporter: Reporter,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, reporter: Reporter) -> Self {
        Self {
            tokens,
            current: 0,
            reporter,
        }
    }

    pub fn parse(&mut self) -> Result<Expr> {
        self.expression()
    }

    fn expression(&mut self) -> Result<Expr> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr> {
        let mut expr = self.comparison()?;

        while self._match(&[Kind::BANG_EQUAL, Kind::EQUAL_EQUAL]) {
            let op = self.previous();
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(BinaryExpression {
                left: expr,
                op,
                right: right,
            }));
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr> {
        let mut expr = self.term()?;

        while self._match(&[
            Kind::GREATER,
            Kind::GREATER_EQUAL,
            Kind::LESS,
            Kind::LESS_EQUAL,
        ]) {
            let op = self.previous();
            let right = self.term()?;
            expr = Expr::Binary(Box::new(BinaryExpression {
                left: expr,
                op,
                right: right,
            }));
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr> {
        let mut expr = self.factor()?;

        while self._match(&[Kind::MINUS, Kind::PLUS]) {
            let op = self.previous();
            let right = self.factor()?;
            expr = Expr::Binary(Box::new(BinaryExpression {
                left: expr,
                op,
                right: right,
            }));
        }
        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr> {
        let mut expr = self.unary()?;

        while self._match(&[Kind::SLASH, Kind::STAR]) {
            let op = self.previous();
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(BinaryExpression {
                left: expr,
                op,
                right: right,
            }));
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr> {
        if self._match(&[Kind::BANG, Kind::MINUS]) {
            let op = self.previous();
            let right = self.unary()?;
            return Ok(Expr::Unary(Box::new(UnaryExpression { op, right: right })));
        }
        self.primary()
    }

    fn primary(&mut self) -> Result<Expr> {
        if self._match(&[Kind::FALSE]) {
            return Ok(Expr::Literal(Box::new(LiteralExpression {
                value: Object::BOOL(false),
            })));
        }
        if self._match(&[Kind::TRUE]) {
            return Ok(Expr::Literal(Box::new(LiteralExpression {
                value: Object::BOOL(true),
            })));
        }
        if self._match(&[Kind::NIL]) {
            return Ok(Expr::Literal(Box::new(LiteralExpression {
                value: Object::NIL(()),
            })));
        }

        if self._match(&[Kind::NUMBER]) {
            return Ok(Expr::Literal(Box::new(LiteralExpression {
                value: Object::NUMBER(self.previous().lexeme.parse::<f64>().unwrap()),
            })));
        }

        if self._match(&[Kind::STRING]) {
            return Ok(Expr::Literal(Box::new(LiteralExpression {
                value: Object::STRING(self.previous().lexeme),
            })));
        }

        if self._match(&[Kind::LEFT_PAREN]) {
            let expr = self.expression()?;
            self.consume(Kind::RIGHT_PAREN, "Expect ')' after expression.")?;
            return Ok(Expr::Grouping(Box::new(GroupingExpression {
                expression: expr,
            })));
        }

        Err(self.error(self.peek(), "Expect expression."))
        // panic!("Expect expression."
    }

    fn consume(&mut self, kind: Kind, message: &str) -> Result<Token> {
        if self.check(&kind) {
            return Ok(self.advance());
        }

        Err(self.error(self.peek(), message))
        // panic!("{}", message.to_string());
    }

    fn error(&mut self, token: Token, message: &str) -> SyntaxError {
        self.reporter.error_token(&token, message);
        SyntaxError::ParserError(message.to_string())
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().kind == Kind::SEMICOLON {
                return;
            }

            match self.peek().kind {
                Kind::CLASS
                | Kind::FUN
                | Kind::VAR
                | Kind::FOR
                | Kind::IF
                | Kind::WHILE
                | Kind::PRINT
                | Kind::RETURN => {
                    return;
                }
                _ => {}
            }

            self.advance();
        }
    }

    fn _match(&mut self, kinds: &[Kind]) -> bool {
        for kind in kinds {
            if self.check(kind) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check(&self, kind: &Kind) -> bool {
        if self.is_at_end() {
            return false;
        }
        self.peek().kind == *kind
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().kind == Kind::EOF
    }

    fn peek(&self) -> Token {
        self.tokens[self.current].clone()
    }

    fn previous(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }
}
