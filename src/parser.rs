/*

==================== Grammar ====================
program        → declaration* EOF ;

declaration    → classDecl
               | funDecl
               | varDecl
               | statement ;

classDecl      → "class" IDENTIFIER ( "<" IDENTIFIER )?
                "{" function* "}" ;;

funDecl        → "fun" function ;
function       → IDENTIFIER "(" parameters? ")" block ;
parameters     → IDENTIFIER ( "," IDENTIFIER )* ;

varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;

statement      → exprStmt
               | forStmt
               | ifStmt
               | printStmt
               | returnStmt
               | whileStmt
               | block ;

returnStmt     → "return" expression? ";" ;

ifStmt         → "if" "(" expression ")" statement
               ( "else" statement )? ;

whileStmt      → "while" "(" expression ")" statement ;

forStmt        → "for" "(" ( varDecl | exprStmt | ";" )
                 expression? ";"
                 expression? ")" statement ;

block          → "{" declaration* "}" ;

exprStmt       → expression ";" ;
printStmt      → "print" expression ";" ;

expression     → assignment ;
assignment     → ( call "." )? IDENTIFIER "=" assignment
               | logic_or ;
logic_or       → logic_and ( "or" logic_and )* ;
logic_and      → equality ( "and" equality )* ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary | call ;
call           → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
primary        → "true" | "false" | "nil" | "this"
               | NUMBER | STRING | IDENTIFIER | "(" expression ")"
               | "super" "." IDENTIFIER ;;
==================== Grammar ====================

*/

use crate::ast::*;
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

    pub fn parse(&mut self) -> Result<Vec<Stmt>> {
        let mut statements: Vec<Stmt> = Vec::new();
        while !self.is_at_end() {
            let r = self.declaration();
            match r {
                Ok(stmt) => statements.push(stmt),
                Err(_) => self.synchronize(),
            }
        }

        Ok(statements)
    }

    fn declaration(&mut self) -> Result<Stmt> {
        if self._match(&[Kind::CLASS]) {
            return self.class_declaration();
        }
        if self._match(&[Kind::FUN]) {
            return self.function("function");
        }
        if self._match(&[Kind::VAR]) {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn class_declaration(&mut self) -> Result<Stmt> {
        let name = self.consume(Kind::IDENTIFIER, "Expect class name.")?;

        let superclass = if self._match(&[Kind::LESS]) {
            self.consume(Kind::IDENTIFIER, "Expect superclass name.")?;
            Some(Expr::Variable(Box::new(VariableExpression {
                name: self.previous(),
                distance: None,
            })))
        } else {
            None
        };

        self.consume(Kind::LEFT_BRACE, "Expect '{' before class body.")?;

        let mut methods: Vec<FunctionStatement> = Vec::new();
        loop {
            if self.check(&Kind::RIGHT_BRACE) || self.is_at_end() {
                break;
            }
            let function = self.function("method")?;
            if let Stmt::Function(f) = function {
                methods.push(*f);
            }
        }
        self.consume(Kind::RIGHT_BRACE, "Expect '}' after class body.")?;
        Ok(Stmt::Class(Box::new(ClassStatement {
            name,
            methods,
            superclass,
        })))
    }

    fn var_declaration(&mut self) -> Result<Stmt> {
        let name = self.consume(Kind::IDENTIFIER, "Expect variable name.")?;

        let initializer = if self._match(&[Kind::EQUAL]) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(Kind::SEMICOLON, "Expect ';' after variable declaration.")?;
        Ok(Stmt::Var(Box::new(VarStatement { name, initializer })))
    }

    fn statement(&mut self) -> Result<Stmt> {
        if self._match(&[Kind::FOR]) {
            return self.for_statement();
        }
        if self._match(&[Kind::IF]) {
            return self.if_statement();
        }
        if self._match(&[Kind::PRINT]) {
            return self.print_statement();
        }
        if self._match(&[Kind::RETURN]) {
            return self.return_statement();
        }
        if self._match(&[Kind::WHILE]) {
            return self.while_statement();
        }
        if self._match(&[Kind::LEFT_BRACE]) {
            let statements = self.block()?;
            return Ok(Stmt::Block(Box::new(BlockStatement { statements })));
        }
        self.expression_statement()
    }

    fn for_statement(&mut self) -> Result<Stmt> {
        self.consume(Kind::LEFT_PAREN, "Expect '(' after 'for'.")?;

        let initializer = if self._match(&[Kind::SEMICOLON]) {
            None
        } else if self._match(&[Kind::VAR]) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        let condition = if !self.check(&Kind::SEMICOLON) {
            self.expression()?
        } else {
            Expr::Literal(Box::new(LiteralExpression {
                value: Literal::Bool(true),
            }))
        };
        self.consume(Kind::SEMICOLON, "Expect ';' after loop condition.")?;

        let increment = if !self.check(&Kind::RIGHT_PAREN) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(Kind::RIGHT_PAREN, "Expect ')' after for clauses.")?;

        let mut body = self.statement()?;

        if let Some(inc) = increment {
            body = Stmt::Block(Box::new(BlockStatement {
                statements: vec![
                    body,
                    Stmt::Expression(Box::new(ExpressionStatement { expression: inc })),
                ],
            }));
        }

        body = Stmt::While(Box::new(WhileStatement { condition, body }));

        if let Some(init) = initializer {
            body = Stmt::Block(Box::new(BlockStatement {
                statements: vec![init, body],
            }));
        }

        Ok(body)
    }

    fn if_statement(&mut self) -> Result<Stmt> {
        self.consume(Kind::LEFT_PAREN, "Expect '(' after 'if'.")?;
        let condition = self.expression()?;
        self.consume(Kind::RIGHT_PAREN, "Expect ')' after if condition.")?;

        let then_branch = self.statement()?;
        let else_branch = if self._match(&[Kind::ELSE]) {
            Some(self.statement()?)
        } else {
            None
        };

        Ok(Stmt::If(Box::new(IfStatement {
            condition,
            then_branch,
            else_branch,
        })))
    }

    fn while_statement(&mut self) -> Result<Stmt> {
        self.consume(Kind::LEFT_PAREN, "Expect '(' after 'while'.")?;
        let condition = self.expression()?;
        self.consume(Kind::RIGHT_PAREN, "Expect ')' after while condition.")?;

        let body = self.statement()?;
        Ok(Stmt::While(Box::new(WhileStatement { condition, body })))
    }

    fn block(&mut self) -> Result<Vec<Stmt>> {
        let mut statements: Vec<Stmt> = Vec::new();

        while !self.check(&Kind::RIGHT_BRACE) && !self.is_at_end() {
            let stmt = self.declaration()?;
            statements.push(stmt);
        }

        self.consume(Kind::RIGHT_BRACE, "Expect '}' after block.")?;
        Ok(statements)
    }

    fn print_statement(&mut self) -> Result<Stmt> {
        let value = self.expression()?;
        self.consume(Kind::SEMICOLON, "Expect ';' after value.")?;
        Ok(Stmt::Print(Box::new(PrintStatement { expression: value })))
    }

    fn return_statement(&mut self) -> Result<Stmt> {
        let keyword = self.previous();
        let value = if !self.check(&Kind::SEMICOLON) {
            self.expression()?
        } else {
            Expr::Literal(Box::new(LiteralExpression {
                value: Literal::Nil(()),
            }))
        };
        self.consume(Kind::SEMICOLON, "Expect ';' after return value.")?;
        Ok(Stmt::Return(Box::new(ReturnStatement { keyword, value })))
    }

    fn expression_statement(&mut self) -> Result<Stmt> {
        let value = self.expression()?;
        self.consume(Kind::SEMICOLON, "Expect ';' after value.")?;
        Ok(Stmt::Expression(Box::new(ExpressionStatement {
            expression: value,
        })))
    }

    fn function(&mut self, kind: &str) -> Result<Stmt> {
        let name = self.consume(Kind::IDENTIFIER, &format!("Expect {kind} name."))?;
        self.consume(Kind::LEFT_PAREN, &format!("Expect '(' after {kind} name."))?;

        let mut params: Vec<Token> = Vec::new();
        if !self.check(&Kind::RIGHT_PAREN) {
            loop {
                if params.len() >= 255 {
                    self.error(self.peek(), "Can't have more than 255 parameters.");
                }

                params.push(self.consume(Kind::IDENTIFIER, "Expect parameter name.")?);
                if !self._match(&[Kind::COMMA]) {
                    break;
                }
            }
        }
        self.consume(Kind::RIGHT_PAREN, "Expect ')' after parameters.")?;

        self.consume(
            Kind::LEFT_BRACE,
            &format!("Expect '{{' before {kind} body."),
        )?;
        let body = self.block()?;
        let stmt = Stmt::Function(Box::new(FunctionStatement { name, params, body }));
        Ok(stmt)
    }

    fn expression(&mut self) -> Result<Expr> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr> {
        let expr = self.or()?;

        if self._match(&[Kind::EQUAL]) {
            let equals = self.previous();
            let value = self.assignment()?;

            match expr {
                Expr::Variable(v) => {
                    let name = v.name;
                    return Ok(Expr::Assign(Box::new(AssignExpression {
                        name,
                        value,
                        distance: None,
                    })));
                }
                Expr::Get(g) => {
                    let object = g.object;
                    let name = g.name;
                    return Ok(Expr::Set(Box::new(SetExpression {
                        object,
                        name,
                        value,
                    })));
                }
                _ => return Err(self.error(equals, "Invalid assignment target.")),
            }
        }

        Ok(expr)
    }

    fn or(&mut self) -> Result<Expr> {
        let mut expr = self.and()?;

        while self._match(&[Kind::OR]) {
            let op = self.previous();
            let right = self.and()?;
            expr = Expr::Logical(Box::new(BinaryExpression {
                left: expr,
                op,
                right,
            }));
        }
        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr> {
        let mut expr = self.equality()?;

        while self._match(&[Kind::AND]) {
            let op = self.previous();
            let right = self.equality()?;
            expr = Expr::Logical(Box::new(BinaryExpression {
                left: expr,
                op,
                right,
            }));
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr> {
        let mut expr = self.comparison()?;

        while self._match(&[Kind::BANG_EQUAL, Kind::EQUAL_EQUAL]) {
            let op = self.previous();
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(BinaryExpression {
                left: expr,
                op,
                right,
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
                right,
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
                right,
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
                right,
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
        self.call()
    }

    fn call(&mut self) -> Result<Expr> {
        let mut expr = self.primary()?;

        loop {
            if self._match(&[Kind::LEFT_PAREN]) {
                expr = self.finish_call(expr)?;
            } else if self._match(&[Kind::DOT]) {
                let name = self.consume(Kind::IDENTIFIER, "Expect property name after '.'.")?;
                expr = Expr::Get(Box::new(GetExpression { object: expr, name }));
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr> {
        let mut arguments: Vec<Expr> = Vec::new();
        if !self.check(&Kind::RIGHT_PAREN) {
            loop {
                if arguments.len() >= 255 {
                    self.error(self.peek(), "Can't have more than 255 arguments.");
                }

                arguments.push(self.expression()?);
                if !self._match(&[Kind::COMMA]) {
                    break;
                }
            }
        }

        let paren = self.consume(Kind::RIGHT_PAREN, "Expect ')' after arguments.")?;

        Ok(Expr::Call(Box::new(CallExpression {
            callee,
            paren,
            arguments,
        })))
    }

    fn primary(&mut self) -> Result<Expr> {
        if self._match(&[Kind::FALSE]) {
            return Ok(Expr::Literal(Box::new(LiteralExpression {
                value: Literal::Bool(false),
            })));
        }
        if self._match(&[Kind::TRUE]) {
            return Ok(Expr::Literal(Box::new(LiteralExpression {
                value: Literal::Bool(true),
            })));
        }
        if self._match(&[Kind::NIL]) {
            return Ok(Expr::Literal(Box::new(LiteralExpression {
                value: Literal::Nil(()),
            })));
        }

        if self._match(&[Kind::NUMBER]) {
            return Ok(Expr::Literal(Box::new(LiteralExpression {
                value: Literal::Num(self.previous().lexeme.parse::<f64>().unwrap()),
            })));
        }

        if self._match(&[Kind::STRING]) {
            return Ok(Expr::Literal(Box::new(LiteralExpression {
                value: Literal::Str(self.previous().lexeme),
            })));
        }

        if self._match(&[Kind::SUPER]) {
            let keyword = self.previous();
            self.consume(Kind::DOT, "Expect '.' after 'super'.")?;
            let method = self.consume(Kind::IDENTIFIER, "Expect superclass method name.")?;
            return Ok(Expr::Super(Box::new(SuperExpression {
                keyword,
                method,
                distance: None,
            })));
        }

        if self._match(&[Kind::THIS]) {
            return Ok(Expr::This(Box::new(ThisExpression {
                keyword: self.previous(),
                distance: None,
            })));
        }

        if self._match(&[Kind::IDENTIFIER]) {
            return Ok(Expr::Variable(Box::new(VariableExpression {
                name: self.previous(),
                distance: None,
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
