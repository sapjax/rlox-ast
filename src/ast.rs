/*

==================== AST ====================
expression     → literal
               | unary
               | binary
               | variable
               | assign
               | grouping ;

literal        → NUMBER | STRING | "true" | "false" | "nil" ;
grouping       → "(" expression ")" ;
unary          → ( "-" | "!" ) expression ;
binary         → expression operator expression ;
logical        → expression ( "and" | "or" ) expression ;
variable       → IDENTIFIER ;

assignment     → IDENTIFIER "=" assignment
operator       → "==" | "!=" | "<" | "<=" | ">" | ">="
               | "+"  | "-"  | "*" | "/" ;

==================== AST ====================
*/

use crate::token::Token;
use serde::{Deserialize, Serialize};

#[derive(Debug, Deserialize, Serialize)]
pub enum Expr {
    Binary(Box<BinaryExpression>),
    Unary(Box<UnaryExpression>),
    Grouping(Box<GroupingExpression>),
    Literal(Box<LiteralExpression>),
    Logical(Box<BinaryExpression>),
    Variable(Box<VariableExpression>),
    Assign(Box<AssignExpression>),
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Binary(binary) | Expr::Logical(binary) => {
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
            Expr::Assign(assign) => {
                write!(f, "(= {} {})", assign.name.lexeme, assign.value)
            }
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct BinaryExpression {
    pub op: Token,
    pub left: Expr,
    pub right: Expr,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct UnaryExpression {
    pub op: Token,
    pub right: Expr,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct GroupingExpression {
    pub expression: Expr,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct VariableExpression {
    pub name: Token,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct AssignExpression {
    pub name: Token,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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

#[derive(Debug, Serialize, Deserialize)]
pub struct LiteralExpression {
    pub value: Object,
}

#[derive(Debug, Serialize, Deserialize)]
pub enum Stmt {
    Block(Box<BlockStatement>),
    Expression(Box<ExpressionStatement>),
    If(Box<IfStatement>),
    Print(Box<PrintStatement>),
    Var(Box<VarStatement>),
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ExpressionStatement {
    pub expression: Expr,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct IfStatement {
    pub condition: Expr,
    pub then_branch: Stmt,
    pub else_branch: Option<Stmt>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct PrintStatement {
    pub expression: Expr,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct VarStatement {
    pub name: Token,
    pub initializer: Option<Expr>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct BlockStatement {
    pub statements: Vec<Stmt>,
}
