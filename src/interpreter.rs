use crate::{
    ast::*,
    object::*,
    reporter::SyntaxError,
    resolver::{Resolvable, Resolver},
    token::{Kind, Token},
};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

pub type Result<T> = std::result::Result<T, SyntaxError>;

pub struct Interpreter {
    pub environment: Rc<RefCell<Environment>>,
    pub globals: Rc<RefCell<Environment>>,
    pub resolver: Resolver,
}

impl Interpreter {
    pub fn new(resolver: Resolver) -> Self {
        let globals = Rc::new(RefCell::new(Environment::new()));
        let environment = Environment::new_child(&globals);

        // define native fn clock
        {
            let now = SystemTime::now();
            let since_the_epoch = now.duration_since(UNIX_EPOCH).expect("Time went backwards");
            let milliseconds = since_the_epoch.as_millis();

            let token = Token::new(Kind::IDENTIFIER, "clock".to_string(), 0);

            let clock_fn = LoxFunction::new(
                FunctionStatement {
                    name: token.clone(),
                    params: vec![],
                    body: vec![Stmt::Return(Box::new(ReturnStatement {
                        keyword: Token::new(Kind::RETURN, "return".to_string(), 0),
                        value: Expr::Literal(Box::new(LiteralExpression {
                            value: Literal::Num(milliseconds as f64),
                        })),
                    }))],
                },
                None,
            );

            globals.borrow_mut().define(token, Obj::Function(clock_fn));
        }

        Self {
            environment: Rc::new(RefCell::new(environment)),
            globals: globals,
            resolver,
        }
    }

    pub fn interpret(&mut self, statements: &mut Vec<Stmt>) -> Result<()> {
        for statement in statements {
            self.execute(statement)?;
        }
        Ok(())
    }

    fn execute(&mut self, stmt: &mut Stmt) -> Result<()> {
        match stmt {
            Stmt::Print(print) => self.visit_print_stmt(print),
            Stmt::Expression(expression) => self.visit_expression_stmt(expression),
            Stmt::Var(var) => self.visit_var_stmt(var),
            Stmt::Block(block) => self.visit_block_stmt(block),
            Stmt::If(if_stmt) => self.visit_if_stmt(if_stmt),
            Stmt::While(while_stmt) => self.visit_while_stmt(while_stmt),
            Stmt::Function(function) => self.visit_function_stmt(*function.clone()), // TODO: use mutable reference
            Stmt::Return(return_stmt) => self.visit_return_stmt(return_stmt),
        }
    }

    pub fn execute_block(
        &mut self,
        statements: &mut Vec<Stmt>,
        block_env: Environment,
    ) -> Result<()> {
        let parent_env = self.environment.clone();
        // set current environment to the new environment
        self.environment = Rc::new(RefCell::new(block_env));

        let mut result = Ok(());
        for statement in statements {
            if let Err(e) = self.execute(statement) {
                result = Err(e);
                break;
            }
        }
        // set current environment back to the parent environment
        self.environment = parent_env;
        result
    }

    fn visit_block_stmt(&mut self, stmt: &mut BlockStatement) -> Result<()> {
        let block_env = Environment::new_child(&self.environment.clone());
        self.execute_block(&mut stmt.statements, block_env)
    }

    fn visit_print_stmt(&mut self, stmt: &mut PrintStatement) -> Result<()> {
        let value = self.evaluate(&mut stmt.expression)?;
        println!("{}", value);
        Ok(())
    }

    fn visit_return_stmt(&mut self, stmt: &mut ReturnStatement) -> Result<()> {
        let value = self.evaluate(&mut stmt.value)?;
        Err(SyntaxError::Return(value))
    }

    fn visit_var_stmt(&mut self, stmt: &mut VarStatement) -> Result<()> {
        let value = match &mut stmt.initializer {
            Some(expr) => self.evaluate(expr)?,
            None => Obj::Nil,
        };

        self.environment
            .borrow_mut()
            .define(stmt.name.clone(), value);
        Ok(())
    }

    fn visit_expression_stmt(&mut self, stmt: &mut ExpressionStatement) -> Result<()> {
        self.evaluate(&mut stmt.expression)?;
        Ok(())
    }

    fn visit_function_stmt(&mut self, stmt: FunctionStatement) -> Result<()> {
        let fu_name = stmt.name.clone();
        let function = LoxFunction::new(stmt, Some(self.environment.clone()));
        self.environment
            .borrow_mut()
            .define(fu_name, Obj::Function(function));
        Ok(())
    }

    fn visit_if_stmt(&mut self, stmt: &mut IfStatement) -> Result<()> {
        if Self::is_truthy(self.evaluate(&mut stmt.condition)?) {
            self.execute(&mut stmt.then_branch)?
        } else if let Some(else_branch) = &mut stmt.else_branch {
            self.execute(else_branch)?
        }
        Ok(())
    }

    fn visit_while_stmt(&mut self, stmt: &mut WhileStatement) -> Result<()> {
        while Self::is_truthy(self.evaluate(&mut stmt.condition)?) {
            self.execute(&mut stmt.body)?;
        }
        Ok(())
    }

    fn visit_assign_expr(&mut self, expr: &mut AssignExpression) -> Result<Obj> {
        let distance = expr.get_distance();
        let value = self.evaluate(&mut expr.value)?;

        if let Some(d) = distance {
            self.environment
                .borrow_mut()
                .assign_at(d, expr.name.clone(), value.clone());
        } else {
            self.globals
                .borrow_mut()
                .assign(expr.name.clone(), value.clone());
        }
        Ok(value)
    }

    fn visit_literal_expr(&self, expr: &mut LiteralExpression) -> Result<Obj> {
        match &expr.value {
            Literal::Str(str) => Ok(Obj::Str(str.clone())),
            Literal::Bool(b) => Ok(Obj::Bool(*b)),
            Literal::Num(n) => Ok(Obj::Num(*n)),
            Literal::Nil(_) => Ok(Obj::Nil),
        }
    }

    fn visit_logical_expr(&mut self, expr: &mut BinaryExpression) -> Result<Obj> {
        let left = self.evaluate(&mut expr.left)?;
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

        self.evaluate(&mut expr.right)
    }

    fn visit_grouping_expr(&mut self, expr: &mut GroupingExpression) -> Result<Obj> {
        self.evaluate(&mut expr.expression)
    }

    fn visit_unary_expr(&mut self, expr: &mut UnaryExpression) -> Result<Obj> {
        let right = self.evaluate(&mut expr.right)?;

        match expr.op.kind {
            Kind::BANG => Ok(Obj::Bool(!Self::is_truthy(right))),
            Kind::MINUS => match right {
                Obj::Num(n) => Ok(Obj::Num(-n)),
                _ => Err(Self::runtime_error(
                    expr.op.clone(),
                    "Operand must be a number",
                )),
            },
            // Unreachable.
            _ => Ok(Obj::Nil),
        }
    }

    fn visit_var_expr(&self, expr: &mut VariableExpression) -> Result<Obj> {
        self.lookup_variable(expr)
    }

    fn lookup_variable(&self, expr: &mut impl Resolvable) -> Result<Obj> {
        let distance = expr.get_distance();
        let name = expr.name();
        if let Some(d) = distance {
            return self
                .environment
                .borrow()
                .get_at(d, name.clone())
                .ok_or_else(|| Self::runtime_error(name.clone(), "Undefined variable"));
        } else {
            return self
                .globals
                .borrow()
                .get(name.clone())
                .ok_or_else(|| Self::runtime_error(name.clone(), "Undefined variable"));
        }
    }

    fn visit_binary_expr(&mut self, expr: &mut BinaryExpression) -> Result<Obj> {
        let left = self.evaluate(&mut expr.left)?;
        let right = self.evaluate(&mut expr.right)?;
        let lexeme = expr.op.lexeme.clone();

        match (left, right) {
            (Obj::Num(l), Obj::Num(r)) => match expr.op.kind {
                Kind::MINUS => Ok(Obj::Num(l - r)),
                Kind::SLASH => Ok(Obj::Num(l / r)),
                Kind::STAR => Ok(Obj::Num(l * r)),
                Kind::PLUS => Ok(Obj::Num(l + r)),
                Kind::GREATER => Ok(Obj::Bool(l > r)),
                Kind::GREATER_EQUAL => Ok(Obj::Bool(l >= r)),
                Kind::LESS => Ok(Obj::Bool(l < r)),
                Kind::LESS_EQUAL => Ok(Obj::Bool(l <= r)),
                Kind::BANG_EQUAL => Ok(Obj::Bool(l != r)),
                Kind::EQUAL_EQUAL => Ok(Obj::Bool(l == r)),
                // Unreachable.
                _ => Err(Self::runtime_error(
                    expr.op.clone(),
                    &format!("mismatched types for binary operator {l} {lexeme} {r}"),
                )),
            },
            (Obj::Str(l), Obj::Str(r)) => match expr.op.kind {
                Kind::PLUS => Ok(Obj::Str(format!("{}{}", l, r))),
                Kind::BANG_EQUAL => Ok(Obj::Bool(l.ne(&r))),
                Kind::EQUAL_EQUAL => Ok(Obj::Bool(l.eq(&r))),
                _ => Err(Self::runtime_error(
                    expr.op.clone(),
                    &format!("mismatched types for binary operator {l} {lexeme} {r}"),
                )),
            },
            (l, r) => match expr.op.kind {
                Kind::BANG_EQUAL => Ok(Obj::Bool(!self.is_equal(l, r))),
                Kind::EQUAL_EQUAL => Ok(Obj::Bool(self.is_equal(l, r))),
                // Unreachable.
                _ => Err(Self::runtime_error(
                    expr.op.clone(),
                    &format!("mismatched types for binary operator {l} {lexeme} {r}"),
                )),
            },
        }
    }

    fn visit_call_expr(&mut self, expr: &mut CallExpression) -> Result<Obj> {
        let callee = self.evaluate(&mut expr.callee)?;

        let mut arguments: Vec<Obj> = Vec::new();
        for argument in &mut expr.arguments {
            arguments.push(self.evaluate(argument)?);
        }

        let mut function = match callee {
            Obj::Function(callable) => callable,
            _ => {
                return Err(Self::runtime_error(
                    expr.paren.clone(),
                    "Can only call functions and classes.",
                ))
            }
        };

        if arguments.len() != function.arity() {
            return Err(Self::runtime_error(
                expr.paren.clone(),
                &format!(
                    "Expected {} arguments but got {}",
                    function.arity(),
                    arguments.len()
                ),
            ));
        }

        Ok(function.call(self, arguments)?)
    }

    fn runtime_error(token: Token, message: &str) -> SyntaxError {
        println!(
            "Error: [line {}] '{}' {}",
            token.line, token.lexeme, message
        );
        SyntaxError::RuntimeError(message.to_string())
    }

    fn is_equal(&self, a: Obj, b: Obj) -> bool {
        match (a, b) {
            (Obj::Nil, Obj::Nil) => true,
            (Obj::Nil, _) => false,
            (_, Obj::Nil) => false,
            (Obj::Num(a), Obj::Num(b)) => a == b,
            (Obj::Str(a), Obj::Str(b)) => a == b,
            (Obj::Bool(a), Obj::Bool(b)) => a == b,
            _ => false,
        }
    }

    fn evaluate(&mut self, expr: &mut Expr) -> Result<Obj> {
        match expr {
            Expr::Literal(literal) => self.visit_literal_expr(literal),
            Expr::Grouping(grouping) => self.visit_grouping_expr(grouping),
            Expr::Unary(unary) => self.visit_unary_expr(unary),
            Expr::Binary(binary) => self.visit_binary_expr(binary),
            Expr::Variable(variable) => self.visit_var_expr(variable),
            Expr::Assign(assign) => self.visit_assign_expr(assign),
            Expr::Logical(logical) => self.visit_logical_expr(logical),
            Expr::Call(call) => self.visit_call_expr(call),
        }
    }

    fn is_truthy(value: Obj) -> bool {
        match value {
            Obj::Bool(b) => b,
            Obj::Nil => false,
            _ => true,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    values: HashMap<String, Obj>,
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

    pub fn define(&mut self, name: Token, value: Obj) {
        self.values.insert(name.lexeme, value);
    }

    pub fn get(&self, name: Token) -> Option<Obj> {
        if let Some(value) = self.values.get(&name.lexeme) {
            return Some(value.clone());
        }

        match &self.parent {
            Some(parent) => parent.borrow().get(name).clone(),
            None => None,
        }
    }

    pub fn get_at(&self, distance: usize, name: Token) -> Option<Obj> {
        match self.ancestor(distance) {
            Some(e) => e.borrow().get(name),
            None => None,
        }
    }

    pub fn ancestor(&self, distance: usize) -> Option<Rc<RefCell<Environment>>> {
        let mut env = Some(Rc::new(RefCell::new(self.clone())));
        for _ in 0..distance {
            env = env.and_then(|e| e.borrow().parent.clone());
        }
        env
    }

    pub fn assign(&mut self, name: Token, value: Obj) -> Option<Obj> {
        if self.values.contains_key(&name.lexeme) {
            self.values.insert(name.lexeme, value)
        } else {
            match &mut self.parent {
                Some(parent) => parent.borrow_mut().assign(name, value),
                None => None,
            }
        }
    }

    pub fn assign_at(&mut self, distance: usize, name: Token, value: Obj) -> Option<Obj> {
        match self.ancestor(distance) {
            Some(e) => e.borrow_mut().assign(name, value),
            None => None,
        }
    }
}
