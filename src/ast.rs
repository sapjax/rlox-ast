/*

==================== AST ====================
expression     → literal
               | unary
               | binary
               | grouping ;

literal        → NUMBER | STRING | "true" | "false" | "nil" ;
grouping       → "(" expression ")" ;
unary          → ( "-" | "!" ) expression ;
binary         → expression operator expression ;
operator       → "==" | "!=" | "<" | "<=" | ">" | ">="
               | "+"  | "-"  | "*" | "/" ;

==================== AST ====================
*/

use crate::token::Token;

#[derive(Debug)]
pub enum Expr {
    Binary(Box<BinaryExpression>),
    Unary(Box<UnaryExpression>),
    Grouping(Box<GroupingExpression>),
    Literal(Box<LiteralExpression>),
    Variable(Box<VariableExpression>),
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Binary(binary) => {
                write!(f, "({} {} {})", binary.op.lexeme, binary.left, binary.right)
            }
            Expr::Unary(unary) => {
                write!(f, "({} {})", unary.op.lexeme, unary.right)
            }
            Expr::Grouping(grouping) => {
                write!(f, "(group {})", grouping.expression)
            }
            Expr::Literal(literal) => {
                write!(f, "{}", literal.value)
            }
            Expr::Variable(variable) => {
                write!(f, "{}", variable.name.lexeme)
            }
        }
    }
}

#[derive(Debug)]
pub struct BinaryExpression {
    pub op: Token,
    pub left: Expr,
    pub right: Expr,
}

#[derive(Debug)]
pub struct UnaryExpression {
    pub op: Token,
    pub right: Expr,
}

#[derive(Debug)]
pub struct GroupingExpression {
    pub expression: Expr,
}

#[derive(Debug)]
pub struct VariableExpression {
    pub name: Token,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    BOOL(bool),
    STRING(String),
    NUMBER(f64),
    NIL(()),
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::BOOL(value) => write!(f, "{}", value),
            Object::STRING(value) => write!(f, "{}", value),
            Object::NUMBER(value) => write!(f, "{}", value),
            Object::NIL(_) => write!(f, "nil"),
        }
    }
}

#[derive(Debug)]
pub struct LiteralExpression {
    pub value: Object,
}

#[derive(Debug)]
pub enum Stmt {
    Expression(Box<ExpressionStatement>),
    Print(Box<PrintStatement>),
    Var(Box<VarStatement>),
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub expression: Expr,
}

#[derive(Debug)]
pub struct PrintStatement {
    pub expression: Expr,
}

#[derive(Debug)]
pub struct VarStatement {
    pub name: Token,
    pub initializer: Option<Expr>,
}
