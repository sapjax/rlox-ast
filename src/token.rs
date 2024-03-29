#![allow(non_camel_case_types)]
use string_cache::DefaultAtom as Atom;

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct Token {
    pub kind: Kind,
    pub line: usize,
    pub lexeme: Atom,
    pub span: Span,
}

impl Token {
    pub fn new(kind: Kind, lexeme: Atom, line: usize) -> Self {
        Self {
            kind,
            lexeme: Atom::from(lexeme),
            line,
            span: Span { start: 0, end: 0 },
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Deserialize, Serialize)]
pub enum Kind {
    // Single-character tokens.
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    // Literals.
    IDENTIFIER,
    STRING,
    NUMBER,

    // Keywords.
    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    EOF,
}

pub fn map_keyword(text: &str) -> Kind {
    match text {
        "and" => Kind::AND,
        "class" => Kind::CLASS,
        "else" => Kind::ELSE,
        "false" => Kind::FALSE,
        "for" => Kind::FOR,
        "fun" => Kind::FUN,
        "if" => Kind::IF,
        "nil" => Kind::NIL,
        "or" => Kind::OR,
        "print" => Kind::PRINT,
        "return" => Kind::RETURN,
        "super" => Kind::SUPER,
        "this" => Kind::THIS,
        "true" => Kind::TRUE,
        "var" => Kind::VAR,
        "while" => Kind::WHILE,
        _ => Kind::IDENTIFIER,
    }
}
