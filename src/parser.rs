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
    BinaryExpression, Expr, GroupingExpression, LiteralExpression, LiteralValue, UnaryExpression,
};
use crate::token::{Kind, Token};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Expr {
        self.expression()
    }

    fn expression(&mut self) -> Expr {
        self.equality()
    }

    fn equality(&mut self) -> Expr {
        let mut expr = self.comparison();

        while self._match(&[Kind::BANG_EQUAL, Kind::EQUAL_EQUAL]) {
            let op = self.previous();
            let right = self.comparison();
            expr = Expr::Binary(Box::new(BinaryExpression {
                left: expr,
                op,
                right: right,
            }));
        }
        expr
    }

    fn comparison(&mut self) -> Expr {
        let mut expr = self.term();

        while self._match(&[
            Kind::GREATER,
            Kind::GREATER_EQUAL,
            Kind::LESS,
            Kind::LESS_EQUAL,
        ]) {
            let op = self.previous();
            let right = self.term();
            expr = Expr::Binary(Box::new(BinaryExpression {
                left: expr,
                op,
                right: right,
            }));
        }
        expr
    }

    fn term(&mut self) -> Expr {
        let mut expr = self.factor();

        while self._match(&[Kind::MINUS, Kind::PLUS]) {
            let op = self.previous();
            let right = self.factor();
            expr = Expr::Binary(Box::new(BinaryExpression {
                left: expr,
                op,
                right: right,
            }));
        }
        expr
    }

    fn factor(&mut self) -> Expr {
        let mut expr = self.unary();

        while self._match(&[Kind::SLASH, Kind::STAR]) {
            let op = self.previous();
            let right = self.unary();
            expr = Expr::Binary(Box::new(BinaryExpression {
                left: expr,
                op,
                right: right,
            }));
        }
        expr
    }

    fn unary(&mut self) -> Expr {
        if self._match(&[Kind::BANG, Kind::MINUS]) {
            let op = self.previous();
            let right = self.unary();
            return Expr::Unary(Box::new(UnaryExpression { op, right: right }));
        }
        self.primary()
    }

    fn primary(&mut self) -> Expr {
        if self._match(&[Kind::FALSE]) {
            return Expr::Literal(Box::new(LiteralExpression {
                value: LiteralValue::FALSE(false),
            }));
        }
        if self._match(&[Kind::TRUE]) {
            return Expr::Literal(Box::new(LiteralExpression {
                value: LiteralValue::TRUE(true),
            }));
        }
        if self._match(&[Kind::NIL]) {
            return Expr::Literal(Box::new(LiteralExpression {
                value: LiteralValue::NIL(()),
            }));
        }

        if self._match(&[Kind::NUMBER]) {
            return Expr::Literal(Box::new(LiteralExpression {
                value: LiteralValue::NUMBER(self.previous().lexeme.parse::<f64>().unwrap()),
            }));
        }

        if self._match(&[Kind::STRING]) {
            return Expr::Literal(Box::new(LiteralExpression {
                value: LiteralValue::STRING(self.previous().lexeme),
            }));
        }

        if self._match(&[Kind::LEFT_PAREN]) {
            let expr = self.expression();
            self.consume(Kind::RIGHT_PAREN, "Expect ')' after expression.");
            return Expr::Grouping(Box::new(GroupingExpression { expression: expr }));
        }

        panic!("Expect expression.");
    }

    fn consume(&mut self, kind: Kind, message: &str) -> Token {
        if self.check(&kind) {
            return self.advance();
        }

        self.error(self.peek(), message);
        panic!("Error");
    }

    fn error(&self, token: Token, message: &str) {
        if token.kind == Kind::EOF {
            println!("Error: [line {}] at end {}", token.line, message);
        } else {
            println!(
                "Error: [line {}] at '{}' {}",
                token.line, token.lexeme, message
            );
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
