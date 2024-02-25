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

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    TRUE(bool),
    FALSE(bool),
    STRING(String),
    NUMBER(f64),
    NIL(()),
}

impl std::fmt::Display for LiteralValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralValue::TRUE(value) => write!(f, "{}", value),
            LiteralValue::FALSE(value) => write!(f, "{}", value),
            LiteralValue::STRING(value) => write!(f, "{}", value),
            LiteralValue::NUMBER(value) => write!(f, "{}", value),
            LiteralValue::NIL(_) => write!(f, "nil"),
        }
    }
}

#[derive(Debug)]
pub struct LiteralExpression {
    pub value: LiteralValue,
}
