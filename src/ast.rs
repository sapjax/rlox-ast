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
call           → expression "(" arguments? ")" ;
logical        → expression ( "and" | "or" ) expression ;
variable       → IDENTIFIER ;
get            → expression "." IDENTIFIER ;
set            → expression "." IDENTIFIER "=" expression ;
this           → "this" ;
super          → "super" "." IDENTIFIER ;

assignment     → IDENTIFIER "=" assignment
operator       → "==" | "!=" | "<" | "<=" | ">" | ">="
               | "+"  | "-"  | "*" | "/" ;

==================== AST ====================
*/

use crate::token::Token;
use serde::{Deserialize, Serialize};
use string_cache::DefaultAtom as Atom;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Stmt {
    Block(Box<BlockStatement>),
    Class(Box<ClassStatement>),
    Expression(Box<ExpressionStatement>),
    Function(Box<FunctionStatement>),
    If(Box<IfStatement>),
    Print(Box<PrintStatement>),
    Return(Box<ReturnStatement>),
    Var(Box<VarStatement>),
    While(Box<WhileStatement>),
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ExpressionStatement {
    pub expression: Expr,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ClassStatement {
    pub name: Token,
    pub superclass: Option<Expr>,
    pub methods: Vec<FunctionStatement>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct FunctionStatement {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct IfStatement {
    pub condition: Expr,
    pub then_branch: Stmt,
    pub else_branch: Option<Stmt>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct PrintStatement {
    pub expression: Expr,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ReturnStatement {
    pub keyword: Token,
    pub value: Expr,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct VarStatement {
    pub name: Token,
    pub initializer: Option<Expr>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct BlockStatement {
    pub statements: Vec<Stmt>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct WhileStatement {
    pub condition: Expr,
    pub body: Stmt,
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq)]
pub enum Expr {
    Binary(Box<BinaryExpression>),
    Unary(Box<UnaryExpression>),
    Grouping(Box<GroupingExpression>),
    Literal(Box<LiteralExpression>),
    Logical(Box<BinaryExpression>),
    Variable(Box<VariableExpression>),
    Assign(Box<AssignExpression>),
    Call(Box<CallExpression>),
    Get(Box<GetExpression>),
    Set(Box<SetExpression>),
    This(Box<ThisExpression>),
    Super(Box<SuperExpression>),
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct BinaryExpression {
    pub op: Token,
    pub left: Expr,
    pub right: Expr,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct UnaryExpression {
    pub op: Token,
    pub right: Expr,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct GroupingExpression {
    pub expression: Expr,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct VariableExpression {
    pub name: Token,
    pub distance: Option<usize>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct AssignExpression {
    pub name: Token,
    pub value: Expr,
    pub distance: Option<usize>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct CallExpression {
    pub callee: Expr,
    pub paren: Token,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct GetExpression {
    pub object: Expr,
    pub name: Token,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct SetExpression {
    pub object: Expr,
    pub name: Token,
    pub value: Expr,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ThisExpression {
    pub keyword: Token,
    pub distance: Option<usize>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct SuperExpression {
    pub keyword: Token,
    pub method: Token,
    pub distance: Option<usize>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct LiteralExpression {
    pub value: Literal,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Literal {
    Bool(bool),
    Str(Atom),
    Num(f64),
    Nil(()),
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
            Expr::Call(call) => {
                write!(
                    f,
                    "({} {})",
                    call.callee,
                    call.arguments
                        .iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Expr::Get(get) => {
                write!(f, "({} . {})", get.object, get.name.lexeme)
            }
            Expr::Set(set) => {
                write!(f, "({} . {} = {})", set.object, set.name.lexeme, set.value)
            }
            Expr::This(this) => {
                write!(f, "{}", this.keyword.lexeme)
            }
            Expr::Super(super_) => {
                write!(f, "({} . {})", super_.keyword.lexeme, super_.method.lexeme)
            }
        }
    }
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Bool(value) => write!(f, "{}", value),
            Literal::Str(value) => write!(f, "{}", value),
            Literal::Num(value) => write!(f, "{}", value),
            Literal::Nil(_) => write!(f, "nil"),
        }
    }
}
