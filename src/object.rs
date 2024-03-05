use crate::ast::FunctionStatement;
use crate::interpreter::{Environment, Interpreter};
use crate::reporter::SyntaxError;
use std::cell::RefCell;
use std::rc::Rc;

// runtime object
#[derive(Debug, PartialEq, Clone)]
pub enum Obj {
    Nil,
    Bool(bool),
    Num(f64),
    Str(String),
    Function(LoxFunction),
}

impl std::fmt::Display for Obj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Obj::Bool(value) => write!(f, "{}", value),
            Obj::Str(value) => write!(f, "{}", value),
            Obj::Num(value) => write!(f, "{}", value),
            Obj::Nil => write!(f, "nil"),
            Obj::Function(func) => write!(f, "{}", func),
        }
    }
}

pub trait LoxCallable {
    fn arity(&self) -> usize;
    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Obj>) -> Result<Obj, SyntaxError>;
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoxFunction {
    pub declaration: FunctionStatement,
    pub closure: Option<Rc<RefCell<Environment>>>,
}

impl LoxFunction {
    pub fn new(declaration: FunctionStatement, closure: Option<Rc<RefCell<Environment>>>) -> Self {
        Self {
            declaration,
            closure: closure,
        }
    }
}

impl LoxCallable for LoxFunction {
    fn arity(&self) -> usize {
        self.declaration.params.len()
    }

    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Obj>) -> Result<Obj, SyntaxError> {
        let mut environment: Environment = Environment::new_child(&self.closure.clone().unwrap());
        for (i, param) in self.declaration.params.iter().enumerate() {
            environment.define(param.lexeme.clone(), arguments[i].clone());
        }
        let result = interpreter.execute_block(self.declaration.body.clone(), environment);
        match result {
            Err(SyntaxError::Return(v)) => return Ok(v),
            Err(e) => return Err(e),
            _ => Ok(Obj::Nil),
        }
    }
}

impl std::fmt::Display for LoxFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fn {} >", self.declaration.name.lexeme)
    }
}
