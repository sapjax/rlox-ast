use crate::{
    ast::*,
    reporter::SyntaxError,
    token::{Kind, Token},
};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type Result<T> = std::result::Result<T, SyntaxError>;

pub struct Interpreter {
    environment: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            environment: Rc::new(RefCell::new(Environment::new())),
        }
    }

    pub fn interpret(&mut self, statements: Vec<Stmt>) -> Result<()> {
        // self.evaluate(expr)
        for statement in statements {
            self.execute(statement)?;
        }
        Ok(())
    }

    fn execute(&mut self, stmt: Stmt) -> Result<()> {
        match stmt {
            Stmt::Print(print) => self.visit_print_stmt(*print),
            Stmt::Expression(expression) => self.visit_expression_stmt(*expression),
            Stmt::Var(var) => self.visit_var_stmt(*var),
            Stmt::Block(block) => self.visit_block_stmt(*block),
            Stmt::If(if_stmt) => self.visit_if_stmt(*if_stmt),
            Stmt::While(while_stmt) => self.visit_while_stmt(*while_stmt),
        }
    }

    fn execute_block(&mut self, statements: Vec<Stmt>) -> Result<()> {
        for statement in statements {
            self.execute(statement)?
        }
        Ok(())
    }

    fn visit_block_stmt(&mut self, stmt: BlockStatement) -> Result<()> {
        let parent_env = self.environment.clone();
        let block_env = Environment::new_child(&self.environment.clone());

        // set current environment to the new environment
        self.environment = Rc::new(RefCell::new(block_env));
        let result = self.execute_block(stmt.statements);
        // set the environment back to the parent environment
        self.environment = parent_env;
        result
    }

    fn visit_print_stmt(&mut self, stmt: PrintStatement) -> Result<()> {
        let value = self.evaluate(stmt.expression)?;
        println!("{}", value);
        Ok(())
    }

    fn visit_var_stmt(&mut self, stmt: VarStatement) -> Result<()> {
        let value = match stmt.initializer {
            Some(expr) => self.evaluate(expr)?,
            None => Object::NIL(()),
        };

        self.environment
            .borrow_mut()
            .define(stmt.name.lexeme, value);
        Ok(())
    }

    fn visit_expression_stmt(&mut self, stmt: ExpressionStatement) -> Result<()> {
        self.evaluate(stmt.expression)?;
        Ok(())
    }

    fn visit_if_stmt(&mut self, stmt: IfStatement) -> Result<()> {
        if Self::is_truthy(self.evaluate(stmt.condition)?) {
            self.execute(stmt.then_branch)?
        } else if let Some(else_branch) = stmt.else_branch {
            self.execute(else_branch)?
        }
        Ok(())
    }

    fn visit_while_stmt(&mut self, stmt: WhileStatement) -> Result<()> {
        while Self::is_truthy(self.evaluate(stmt.condition.clone())?) {
            self.execute(stmt.body.clone())?;
        }
        Ok(())
    }

    fn visit_assign_expr(&mut self, expr: AssignExpression) -> Result<Object> {
        let value = self.evaluate(expr.value)?;
        let old_value = self
            .environment
            .borrow_mut()
            .assign(&expr.name.lexeme, value.clone());
        match old_value {
            Some(_) => Ok(value),
            None => Err(Self::runtime_error(expr.name, "Undefined variable")),
        }
    }

    fn visit_literal_expr(&self, expr: LiteralExpression) -> Result<Object> {
        Ok(expr.value)
    }

    fn visit_logical_expr(&mut self, expr: BinaryExpression) -> Result<Object> {
        let left = self.evaluate(expr.left)?;
        let left_is_truthy = Self::is_truthy(left.clone());

        if expr.op.kind == Kind::OR {
            if left_is_truthy {
                return Ok(left);
            }
        } else {
            if !left_is_truthy {
                return Ok(left);
            }
        }

        self.evaluate(expr.right)
    }

    fn visit_grouping_expr(&mut self, expr: GroupingExpression) -> Result<Object> {
        self.evaluate(expr.expression)
    }

    fn visit_unary_expr(&mut self, expr: UnaryExpression) -> Result<Object> {
        let right = self.evaluate(expr.right)?;

        match expr.op.kind {
            Kind::BANG => Ok(Object::BOOL(!Self::is_truthy(right))),
            Kind::MINUS => match right {
                Object::NUMBER(n) => Ok(Object::NUMBER(-n)),
                _ => Err(Self::runtime_error(expr.op, "Operand must be a number")),
            },
            // Unreachable.
            _ => Ok(Object::NIL(())),
        }
    }

    fn visit_var_expr(&self, expr: VariableExpression) -> Result<Object> {
        match self.environment.borrow().get(&expr.name.lexeme) {
            Some(value) => Ok(value.clone()),
            None => Err(Self::runtime_error(expr.name, "Undefined variable")),
        }
    }

    fn visit_binary_expr(&mut self, expr: BinaryExpression) -> Result<Object> {
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
        println!(
            "Error: [line {}] '{}' {}",
            token.line, token.lexeme, message
        );
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

    fn evaluate(&mut self, expr: Expr) -> Result<Object> {
        match expr {
            Expr::Literal(literal) => self.visit_literal_expr(*literal),
            Expr::Grouping(grouping) => self.visit_grouping_expr(*grouping),
            Expr::Unary(unary) => self.visit_unary_expr(*unary),
            Expr::Binary(binary) => self.visit_binary_expr(*binary),
            Expr::Variable(variable) => self.visit_var_expr(*variable),
            Expr::Assign(assign) => self.visit_assign_expr(*assign),
            Expr::Logical(logical) => self.visit_logical_expr(*logical),
        }
    }

    fn is_truthy(value: Object) -> bool {
        match value {
            Object::BOOL(b) => b,
            Object::NIL(()) => false,
            _ => true,
        }
    }
}

#[derive(Clone)]
pub struct Environment {
    values: HashMap<String, Object>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            parent: None,
            values: HashMap::new(),
        }
    }

    pub fn new_child(parent: &Rc<RefCell<Environment>>) -> Self {
        Environment {
            parent: Some(parent.clone()),
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, value: Object) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        if let Some(value) = self.values.get(name) {
            return Some(value.clone());
        }

        match &self.parent {
            Some(parent) => parent.borrow().get(name).clone(),
            None => None,
        }
    }

    pub fn assign(&mut self, name: &str, value: Object) -> Option<Object> {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), value)
        } else {
            match &mut self.parent {
                Some(parent) => parent.borrow_mut().assign(name, value),
                None => None,
            }
        }
    }
}
