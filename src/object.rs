use crate::ast::FunctionStatement;
use crate::interpreter::{Environment, Interpreter};
use crate::reporter::SyntaxError;
use crate::token::Token;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

// runtime object
#[derive(Debug, PartialEq, Clone)]
pub enum Obj {
    Nil,
    Bool(bool),
    Num(f64),
    Str(String),
    Function(LoxFunction),
    NativeFn(NativeFunction),
    Class(LoxClass),
    Instance(Rc<RefCell<LoxInstance>>),
}

impl std::fmt::Display for Obj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Obj::Bool(value) => write!(f, "{}", value),
            Obj::Str(value) => write!(f, "{}", value),
            Obj::Num(value) => write!(f, "{}", value),
            Obj::Nil => write!(f, "nil"),
            Obj::Function(func) => write!(f, "{}", func),
            Obj::NativeFn(native) => write!(f, "<native fn {}>", native.name),
            Obj::Class(class) => write!(f, "{}", class),
            Obj::Instance(instance) => write!(f, "{}", instance.borrow()),
        }
    }
}

pub trait LoxCallable {
    fn arity(&self) -> usize;
    fn call(
        &mut self,
        interpreter: &mut Interpreter,
        arguments: Vec<Obj>,
    ) -> Result<Obj, SyntaxError>;
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoxFunction {
    pub declaration: FunctionStatement,
    pub closure: Option<Rc<RefCell<Environment>>>,
    is_initializer: bool,
}

impl LoxFunction {
    pub fn new(
        declaration: FunctionStatement,
        closure: Option<Rc<RefCell<Environment>>>,
        is_initializer: bool,
    ) -> Self {
        Self {
            declaration,
            closure,
            is_initializer,
        }
    }

    pub fn bind(&self, instance: Rc<RefCell<LoxInstance>>) -> LoxFunction {
        let mut environment = Environment::new_child(&self.closure.as_ref().unwrap());
        environment.define("this".to_string(), Obj::Instance(instance));
        LoxFunction {
            declaration: self.declaration.clone(),
            closure: Some(Rc::new(RefCell::new(environment))),
            is_initializer: self.is_initializer,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NativeFunction {
    pub name: String,
    pub arity: usize,
    pub native_call: fn(Vec<Obj>) -> Obj,
}

impl NativeFunction {
    pub fn new(name: String, arity: usize, native_call: fn(Vec<Obj>) -> Obj) -> Self {
        Self {
            name,
            arity,
            native_call,
        }
    }
}

impl LoxCallable for NativeFunction {
    fn arity(&self) -> usize {
        self.arity
    }

    fn call(
        &mut self,
        _interpreter: &mut Interpreter,
        arguments: Vec<Obj>,
    ) -> Result<Obj, SyntaxError> {
        Ok((self.native_call)(arguments))
    }
}

impl LoxCallable for LoxFunction {
    fn arity(&self) -> usize {
        self.declaration.params.len()
    }

    fn call(
        &mut self,
        interpreter: &mut Interpreter,
        arguments: Vec<Obj>,
    ) -> Result<Obj, SyntaxError> {
        let closure = match &self.closure {
            Some(c) => &c,
            None => &interpreter.globals,
        };

        let mut environment: Environment = Environment::new_child(closure);
        for (i, param) in self.declaration.params.iter().enumerate() {
            environment.define(param.clone().lexeme, arguments[i].clone());
        }
        let result = interpreter.execute_block(&mut self.declaration.body, environment);

        let mut default_value = Obj::Nil;
        if self.is_initializer {
            default_value = self
                .closure
                .as_ref()
                .unwrap()
                .borrow()
                .get_at(0, "this".to_string())
                .unwrap();
        }

        match result {
            Err(SyntaxError::Return(v)) => {
                if self.is_initializer {
                    return Ok(default_value);
                }
                Ok(v)
            }
            Err(e) => Err(e),
            _ => Ok(default_value),
        }
    }
}

impl std::fmt::Display for LoxFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fn {} >", self.declaration.name.lexeme)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct LoxClass {
    pub name: Token,
    super_class: Option<Box<LoxClass>>,
    methods: HashMap<String, LoxFunction>,
}

impl LoxClass {
    pub fn new(
        name: Token,
        super_class: Option<Box<LoxClass>>,
        methods: HashMap<String, LoxFunction>,
    ) -> Self {
        Self {
            name,
            super_class,
            methods,
        }
    }

    pub fn find_method(&self, name: &str) -> Option<LoxFunction> {
        if let Some(method) = self.methods.get(name) {
            return Some(method.clone());
        }

        if let Some(super_class) = &self.super_class {
            return super_class.find_method(name);
        }
        None
    }
}

impl LoxCallable for LoxClass {
    fn arity(&self) -> usize {
        if let Some(initializer) = self.find_method("init") {
            initializer.arity()
        } else {
            0
        }
    }

    fn call(
        &mut self,
        interpreter: &mut Interpreter,
        arguments: Vec<Obj>,
    ) -> Result<Obj, SyntaxError> {
        let instance = Rc::new(RefCell::new(LoxInstance {
            class: self.clone(),
            fields: HashMap::new(),
        }));
        if let Some(initializer) = self.find_method("init") {
            initializer
                .bind(Rc::clone(&instance))
                .call(interpreter, arguments)?;
        }

        Ok(Obj::Instance(instance))
    }
}

impl std::fmt::Display for LoxClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<class {}>", self.name.lexeme)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct LoxInstance {
    class: LoxClass,
    fields: HashMap<String, Obj>,
}

impl LoxInstance {
    pub fn get(name: Token, instance: Rc<RefCell<Self>>) -> Option<Obj> {
        if let Some(value) = instance.borrow().fields.get(&name.lexeme) {
            return Some(value.clone());
        }

        if let Some(method) = instance.borrow().class.find_method(&name.lexeme) {
            return Some(Obj::Function(method.bind(Rc::clone(&instance))));
        }
        None
    }

    pub fn set(&mut self, name: Token, value: Obj) {
        self.fields.insert(name.lexeme, value);
    }
}

impl std::fmt::Display for LoxInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<instance of {}>", self.class)
    }
}
