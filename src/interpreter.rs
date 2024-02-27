use crate::{
    ast::{
        BinaryExpression, Expr, ExpressionStatement, GroupingExpression, LiteralExpression, Object,
        PrintStatement, Stmt, UnaryExpression,
    },
    reporter::SyntaxError,
    token::{Kind, Token},
};

pub type Result<T> = std::result::Result<T, SyntaxError>;

pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn interpret(&self, statements: Vec<Stmt>) -> Result<()> {
        // self.evaluate(expr)
        for statement in statements {
            self.execute(statement)?;
        }
        Ok(())
    }

    fn execute(&self, stmt: Stmt) -> Result<()> {
        match stmt {
            Stmt::Print(print) => self.visit_print_stmt(*print),
            Stmt::Expression(expression) => self.visit_expression_stmt(*expression),
        }
    }

    fn visit_print_stmt(&self, stmt: PrintStatement) -> Result<()> {
        let value = self.evaluate(stmt.expression)?;
        println!("{}", value);
        Ok(())
    }

    fn visit_expression_stmt(&self, stmt: ExpressionStatement) -> Result<()> {
        self.evaluate(stmt.expression)?;
        Ok(())
    }

    fn visit_literal_expr(&self, expr: LiteralExpression) -> Result<Object> {
        Ok(expr.value)
    }

    fn visit_grouping_expr(&self, expr: GroupingExpression) -> Result<Object> {
        self.evaluate(expr.expression)
    }

    fn visit_unary_expr(&self, expr: UnaryExpression) -> Result<Object> {
        let right = self.evaluate(expr.right)?;

        match expr.op.kind {
            Kind::BANG => Ok(Object::BOOL(!self.is_truthy(right))),
            Kind::MINUS => match right {
                Object::NUMBER(n) => Ok(Object::NUMBER(-n)),
                _ => Err(Self::runtime_error(expr.op, "Operand must be a number")),
            },
            // Unreachable.
            _ => Ok(Object::NIL(())),
        }
    }

    fn visit_binary_expr(&self, expr: BinaryExpression) -> Result<Object> {
        let left = self.evaluate(expr.left)?;
        let right = self.evaluate(expr.right)?;
        let lexeme = expr.op.lexeme.clone();

        match (left, right) {
            (Object::NUMBER(l), Object::NUMBER(r)) => match expr.op.kind {
                Kind::MINUS => Ok(Object::NUMBER(l - r)),
                Kind::SLASH => Ok(Object::NUMBER(l / r)),
                Kind::STAR => Ok(Object::NUMBER(l * r)),
                Kind::PLUS => Ok(Object::NUMBER(l + r)),
                Kind::GREATER => Ok(Object::BOOL(l > r)),
                Kind::GREATER_EQUAL => Ok(Object::BOOL(l >= r)),
                Kind::LESS => Ok(Object::BOOL(l < r)),
                Kind::LESS_EQUAL => Ok(Object::BOOL(l <= r)),
                Kind::BANG_EQUAL => Ok(Object::BOOL(l != r)),
                Kind::EQUAL_EQUAL => Ok(Object::BOOL(l == r)),
                // Unreachable.
                _ => Err(Self::runtime_error(
                    expr.op,
                    &format!("mismatched types for binary operator {l} {lexeme} {r}"),
                )),
            },
            (Object::STRING(l), Object::STRING(r)) => match expr.op.kind {
                Kind::PLUS => Ok(Object::STRING(format!("{}{}", l, r))),
                Kind::BANG_EQUAL => Ok(Object::BOOL(l.ne(&r))),
                Kind::EQUAL_EQUAL => Ok(Object::BOOL(l.eq(&r))),
                _ => Err(Self::runtime_error(
                    expr.op,
                    &format!("mismatched types for binary operator {l} {lexeme} {r}"),
                )),
            },
            (l, r) => match expr.op.kind {
                Kind::BANG_EQUAL => Ok(Object::BOOL(!self.is_equal(l, r))),
                Kind::EQUAL_EQUAL => Ok(Object::BOOL(self.is_equal(l, r))),
                // Unreachable.
                _ => Err(Self::runtime_error(
                    expr.op,
                    &format!("mismatched types for binary operator {l} {lexeme} {r}"),
                )),
            },
        }
    }

    fn runtime_error(token: Token, message: &str) -> SyntaxError {
        println!("Error: [line {}] {}", message, token.line);
        SyntaxError::RuntimeError(message.to_string())
    }

    fn is_equal(&self, a: Object, b: Object) -> bool {
        match (a, b) {
            (Object::NIL(()), Object::NIL(())) => true,
            (Object::NIL(()), _) => false,
            (_, Object::NIL(())) => false,
            (Object::NUMBER(a), Object::NUMBER(b)) => a == b,
            (Object::STRING(a), Object::STRING(b)) => a == b,
            (Object::BOOL(a), Object::BOOL(b)) => a == b,
            _ => false,
        }
    }

    fn evaluate(&self, expr: Expr) -> Result<Object> {
        match expr {
            Expr::Literal(literal) => self.visit_literal_expr(*literal),
            Expr::Grouping(grouping) => self.visit_grouping_expr(*grouping),
            Expr::Unary(unary) => self.visit_unary_expr(*unary),
            Expr::Binary(binary) => self.visit_binary_expr(*binary),
        }
    }

    fn is_truthy(&self, value: Object) -> bool {
        match value {
            Object::BOOL(b) => b,
            Object::NIL(()) => false,
            _ => true,
        }
    }
}
