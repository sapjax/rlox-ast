use crate::ast::*;
use crate::reporter::Reporter;
use crate::token::Token;
use std::collections::HashMap;

type Scope = HashMap<String, bool>;

#[derive(Clone, Copy, PartialEq)]
enum FunctionType {
    None,
    Function,
    INITIALIZER,
    METHOD,
}

#[derive(Clone, Copy, PartialEq)]
enum ClassType {
    None,
    Class,
    SUBCLASS,
}

pub struct Resolver {
    scopes: Vec<Scope>,
    reporter: Reporter,
    fn_type: FunctionType,
    class_type: ClassType,
}

impl Resolver {
    pub fn new(reporter: Reporter) -> Self {
        Self {
            scopes: vec![Scope::new()],
            reporter,
            fn_type: FunctionType::None,
            class_type: ClassType::None,
        }
    }

    pub fn resolve_stmts(&mut self, stmts: &mut Vec<Stmt>) -> bool {
        for stmt in stmts {
            self.resolve_stmt(stmt);
        }
        self.reporter.had_error
    }

    fn resolve_stmt(&mut self, stmt: &mut Stmt) {
        match stmt {
            Stmt::Block(stmt) => self.visit_block_stmt(&mut stmt.statements),
            Stmt::Expression(stmt) => self.visit_expression_stmt(stmt),
            Stmt::Function(stmt) => self.visit_fn_stmt(stmt),
            Stmt::If(stmt) => self.visit_if_stmt(stmt),
            Stmt::Print(stmt) => self.visit_print_stmt(stmt),
            Stmt::Return(stmt) => self.visit_return_stmt(stmt),
            Stmt::Var(stmt) => self.visit_var_stmt(stmt),
            Stmt::While(stmt) => self.visit_while_stmt(stmt),
            Stmt::Class(stmt) => self.visit_class_stmt(stmt),
        }
    }

    fn resolve_expr(&mut self, expr: &mut Expr) {
        match expr {
            Expr::Variable(expr) => self.visit_var_expr(expr),
            Expr::Assign(expr) => self.visit_assign_expr(expr),
            Expr::Binary(expr) => self.visit_binary_expr(expr),
            Expr::Call(expr) => self.visit_call_expr(expr),
            Expr::Grouping(expr) => self.visit_grouping_expr(expr),
            Expr::Literal(expr) => self.visit_literal_expr(expr),
            Expr::Logical(expr) => self.visit_logical_expr(expr),
            Expr::Unary(expr) => self.visit_unary_expr(expr),
            Expr::Get(expr) => self.visit_get_expr(expr),
            Expr::Set(expr) => self.visit_set_expr(expr),
            Expr::This(expr) => self.visit_this_expr(expr),
            Expr::Super(expr) => self.visit_super_expr(expr),
        }
    }

    fn visit_block_stmt(&mut self, stmts: &mut Vec<Stmt>) {
        self.begin_scope();
        self.resolve_stmts(stmts);
        self.end_scope();
    }

    fn visit_class_stmt(&mut self, stmt: &mut ClassStatement) {
        let enclosing_class_type = self.class_type;
        self.class_type = ClassType::Class;

        self.declare(stmt.name.clone());
        self.define(stmt.name.clone());

        if let Some(superclass_expr) = &mut stmt.superclass {
            if let Expr::Variable(superclass) = superclass_expr {
                if stmt.name.lexeme == superclass.name.lexeme {
                    self.reporter
                        .error(superclass.name.line, "A class cannot inherit from itself");
                }
            }
            self.class_type = ClassType::SUBCLASS;
            self.resolve_expr(superclass_expr);
        }

        // super
        if let Some(_superclass_expr) = &stmt.superclass {
            self.begin_scope();
            let scope = self.scopes.last_mut().unwrap();
            scope.insert("super".to_string(), true);
        }

        // this
        self.begin_scope();
        let scope = self.scopes.last_mut().unwrap();
        scope.insert("this".to_string(), true);

        for method in &mut stmt.methods {
            let fn_type = if method.name.lexeme == "init" {
                FunctionType::INITIALIZER
            } else {
                FunctionType::METHOD
            };
            self.resolve_fn_stmt(method, fn_type)
        }
        self.end_scope();

        if stmt.superclass.is_some() {
            self.end_scope();
        }

        self.class_type = enclosing_class_type;
    }

    fn visit_expression_stmt(&mut self, expr: &mut ExpressionStatement) {
        self.resolve_expr(&mut expr.expression);
    }

    fn visit_fn_stmt(&mut self, stmt: &mut FunctionStatement) {
        self.declare(stmt.name.clone());
        self.define(stmt.name.clone());
        self.resolve_fn_stmt(stmt, FunctionType::Function);
    }

    fn visit_if_stmt(&mut self, stmt: &mut IfStatement) {
        self.resolve_expr(&mut stmt.condition);
        self.resolve_stmt(&mut stmt.then_branch);
        if let Some(else_branch) = &mut stmt.else_branch {
            self.resolve_stmt(else_branch);
        }
    }

    fn visit_print_stmt(&mut self, stmt: &mut PrintStatement) {
        self.resolve_expr(&mut stmt.expression);
    }

    fn visit_return_stmt(&mut self, stmt: &mut ReturnStatement) {
        if self.fn_type == FunctionType::None {
            self.reporter
                .error(stmt.keyword.line, "Cannot return from top-level code");
        }

        if let Expr::Literal(literal) = &stmt.value {
            if Literal::Nil(()) != literal.value {
                if self.fn_type == FunctionType::INITIALIZER {
                    self.reporter.error(
                        stmt.keyword.line,
                        "Cannot return a value from an initializer",
                    );
                };
            }
        }

        self.resolve_expr(&mut stmt.value);
    }

    fn visit_var_stmt(&mut self, stmt: &mut VarStatement) {
        self.declare(stmt.name.clone());
        if let Some(initializer) = &mut stmt.initializer {
            self.resolve_expr(initializer);
        }
        self.define(stmt.name.clone());
    }

    fn visit_while_stmt(&mut self, stmt: &mut WhileStatement) {
        self.resolve_expr(&mut stmt.condition);
        self.resolve_stmt(&mut stmt.body);
    }

    fn visit_var_expr(&mut self, expr: &mut VariableExpression) {
        if !self.scopes.is_empty() {
            let scope = self.scopes.last().unwrap();
            if let Some(defined) = scope.get(&expr.name.lexeme) {
                if !*defined {
                    self.reporter.error(
                        expr.name.line,
                        &format!("Cannot read local variable in its own initializer"),
                    );
                }
            }

            self.resolve_local(expr);
        }
    }

    fn visit_assign_expr(&mut self, expr: &mut AssignExpression) {
        self.resolve_expr(&mut expr.value);
        self.resolve_local(expr);
    }

    fn visit_binary_expr(&mut self, expr: &mut BinaryExpression) {
        self.resolve_expr(&mut expr.left);
        self.resolve_expr(&mut expr.right);
    }

    fn visit_call_expr(&mut self, expr: &mut CallExpression) {
        self.resolve_expr(&mut expr.callee);
        for arg in &mut expr.arguments {
            self.resolve_expr(arg);
        }
    }

    fn visit_get_expr(&mut self, expr: &mut GetExpression) {
        self.resolve_expr(&mut expr.object);
    }

    fn visit_set_expr(&mut self, expr: &mut SetExpression) {
        self.resolve_expr(&mut expr.value);
        self.resolve_expr(&mut expr.object);
    }

    fn visit_super_expr(&mut self, expr: &mut SuperExpression) {
        if self.class_type == ClassType::None {
            self.reporter
                .error(expr.keyword.line, "Cannot use 'super' outside of a class");
        } else if self.class_type != ClassType::SUBCLASS {
            self.reporter.error(
                expr.keyword.line,
                "Cannot use 'super' in a class with no superclass",
            );
        }

        self.resolve_local(expr);
    }

    fn visit_this_expr(&mut self, expr: &mut ThisExpression) {
        if self.class_type == ClassType::None {
            self.reporter
                .error(expr.keyword.line, "Cannot use 'this' outside of a class");
        }

        self.resolve_local(expr);
    }

    fn visit_grouping_expr(&mut self, expr: &mut GroupingExpression) {
        self.resolve_expr(&mut expr.expression);
    }

    fn visit_literal_expr(&mut self, _expr: &LiteralExpression) {
        // Do nothing
    }

    fn visit_logical_expr(&mut self, expr: &mut BinaryExpression) {
        self.resolve_expr(&mut expr.left);
        self.resolve_expr(&mut expr.right);
    }

    fn visit_unary_expr(&mut self, expr: &mut UnaryExpression) {
        self.resolve_expr(&mut expr.right);
    }

    fn resolve_fn_stmt(&mut self, stmt: &mut FunctionStatement, fn_type: FunctionType) {
        let enclosing_fn_type = self.fn_type;
        self.fn_type = fn_type;

        self.begin_scope();
        for param in &stmt.params {
            self.declare(param.clone());
            self.define(param.clone());
        }
        self.resolve_stmts(&mut stmt.body);
        self.end_scope();
        self.fn_type = enclosing_fn_type;
    }

    fn resolve_local(&mut self, resolvable: &mut impl Resolvable) {
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(&resolvable.name().lexeme) {
                resolvable.set_distance(i);
                return;
            }
        }
    }

    fn declare(&mut self, name: Token) {
        if !self.scopes.is_empty() {
            let scope = self.scopes.last_mut().unwrap();
            if scope.contains_key(&name.lexeme) {
                self.reporter.error(
                    name.line,
                    &format!(
                        "Variable with name {} already declared in this scope",
                        name.lexeme
                    ),
                );
            }
            scope.insert(name.lexeme, false);
        }
    }

    fn define(&mut self, name: Token) {
        if !self.scopes.is_empty() {
            self.scopes.last_mut().unwrap().insert(name.lexeme, true);
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }
}

pub trait Resolvable {
    fn name(&self) -> &Token;
    fn set_distance(&mut self, distance: usize);
    fn get_distance(&self) -> Option<usize>;
}

impl Resolvable for AssignExpression {
    fn name(&self) -> &Token {
        &self.name
    }

    fn set_distance(&mut self, distance: usize) {
        self.distance = Some(distance);
    }

    fn get_distance(&self) -> Option<usize> {
        self.distance
    }
}

impl Resolvable for VariableExpression {
    fn name(&self) -> &Token {
        &self.name
    }

    fn set_distance(&mut self, distance: usize) {
        self.distance = Some(distance);
    }

    fn get_distance(&self) -> Option<usize> {
        self.distance
    }
}

impl Resolvable for ThisExpression {
    fn name(&self) -> &Token {
        &self.keyword
    }

    fn set_distance(&mut self, distance: usize) {
        self.distance = Some(distance);
    }

    fn get_distance(&self) -> Option<usize> {
        self.distance
    }
}

impl Resolvable for SuperExpression {
    fn name(&self) -> &Token {
        &self.keyword
    }

    fn set_distance(&mut self, distance: usize) {
        self.distance = Some(distance);
    }

    fn get_distance(&self) -> Option<usize> {
        self.distance
    }
}
