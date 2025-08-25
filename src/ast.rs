//! This module defines types which represent abstract syntax tree nodes.
//! The root of the AST is a [Prog].

use crate::{
    span::Span,
    token::{Keyword, Token},
};

/// AST Node representing a whole program.
#[derive(Debug, PartialEq, Clone)]
pub struct Prog {
    pub stmts: Vec<Span<Stmt>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Expr {
        expr: Box<Span<Expr>>,
    },
    Let {
        keyword: Span<()>,
        pat: Box<Span<Pattern>>,
        eq_tok: Span<()>,
        init: Box<Span<Expr>>,
    },
    Print {
        keyword: Span<()>,
        expr: Box<Span<Expr>>,
    },
    Block {
        brace_l: Span<()>,
        stmts: Vec<Span<Stmt>>,
        brace_r: Span<()>,
    },
    If {
        keyword: Span<()>,
        condition: Box<Span<Expr>>,
        then_keyword: Span<()>,
        cons_branch: Box<Span<Stmt>>,
        else_keyword: Option<Span<()>>,
        alt_branch: Option<Box<Span<Stmt>>>,
    },
    Return {
        keyword: Span<()>,
        expr: Option<Box<Span<Expr>>>,
    },
    FuncDecl {
        keyword: Span<()>,
        name: Span<String>,
        param_paren_l: Span<()>,
        params: Vec<Span<String>>,
        param_paren_r: Span<()>,
        body: Box<Span<Stmt>>,
    },
    While {
        keyword: Span<()>,
        condition: Box<Span<Expr>>,
        do_keyword: Span<()>,
        body: Box<Span<Stmt>>,
    },
    Break {
        keyword: Span<()>,
    },
    Continue {
        keyword: Span<()>,
    },
}

impl Stmt {
    /// Whether the statement needs a semicolon after it.
    /// Statements which use curly braces do not need a semicolon after the closing brace.
    #[must_use]
    pub fn needs_separator(&self) -> bool {
        match self {
            Stmt::Expr { .. }
            | Stmt::Let { .. }
            | Stmt::Print { .. }
            | Stmt::Return { .. }
            | Stmt::Break { .. }
            | Stmt::Continue { .. } => true,

            Stmt::Block { .. } | Stmt::FuncDecl { .. } => false,

            Stmt::If {
                cons_branch,
                alt_branch,
                ..
            } => {
                if let Some(alt_branch) = alt_branch {
                    alt_branch.node.needs_separator()
                } else {
                    cons_branch.node.needs_separator()
                }
            }

            Stmt::While { body, .. } => body.node.needs_separator(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    Ident {
        name: String,
    },
    Obj {
        brace_l: Span<()>,
        props: Vec<(Span<String>, Span<Pattern>)>,
        brace_r: Span<()>,
    },
    Default {
        pat: Box<Span<Pattern>>,
        eq_tok: Span<()>,
        default: Box<Span<Expr>>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Nil,
    Bool {
        val: bool,
    },
    Num {
        val: f64,
    },
    Str {
        val: String,
    },
    Var {
        name: String,
    },
    UnaryOp {
        op: Span<UnaryOp>,
        expr: Box<Span<Expr>>,
    },
    BinOp {
        left: Box<Span<Expr>>,
        op: Span<BinOp>,
        right: Box<Span<Expr>>,
    },
    CompareChain {
        left: Box<Span<Expr>>,
        comparators: Vec<(Span<CompareOp>, Span<Expr>)>,
    },
    Assign {
        place: Box<Span<Expr>>,
        eq_tok: Span<()>,
        expr: Box<Span<Expr>>,
    },
    Obj {
        brace_l: Span<()>,
        props: Vec<(Span<String>, Span<Expr>)>,
        brace_r: Span<()>,
    },
    PropAccess {
        obj: Box<Span<Expr>>,
        dot_tok: Span<()>,
        prop: Span<String>,
    },
    FuncCall {
        callee: Box<Span<Expr>>,
        paren_l: Span<()>,
        args: Vec<Span<Expr>>,
        paren_r: Span<()>,
    },
    Lambda {
        pipe_l: Span<()>,
        params: Vec<Span<String>>,
        pipe_r: Span<()>,
        body: Box<Span<Stmt>>,
    },
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinOp {
    Or,
    And,
    Add,
    Sub,
    Mul,
    Div,
    Concat,
}

impl BinOp {
    #[must_use]
    pub fn try_from_token(tok: &Span<Token>) -> Option<Self> {
        match &tok.node {
            Token::Keyword(Keyword::Or) => Some(BinOp::Or),
            Token::Keyword(Keyword::And) => Some(BinOp::And),
            Token::Plus => Some(BinOp::Add),
            Token::PlusPlus => Some(BinOp::Concat),
            Token::Minus => Some(BinOp::Sub),
            Token::Star => Some(BinOp::Mul),
            Token::Slash => Some(BinOp::Div),
            _ => None,
        }
    }

    #[must_use]
    pub fn get_precedence(self) -> u8 {
        match self {
            BinOp::Or => 2,
            BinOp::And => 3,
            BinOp::Concat => 5,
            BinOp::Add | BinOp::Sub => 6,
            BinOp::Mul | BinOp::Div => 7,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum CompareOp {
    Eq,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
}

impl CompareOp {
    #[must_use]
    pub fn try_from_token(tok: &Span<Token>) -> Option<Self> {
        match &tok.node {
            Token::EqEq => Some(CompareOp::Eq),
            Token::BangEq => Some(CompareOp::NotEq),
            Token::Less => Some(CompareOp::Less),
            Token::LessEq => Some(CompareOp::LessEq),
            Token::Greater => Some(CompareOp::Greater),
            Token::GreaterEq => Some(CompareOp::GreaterEq),
            _ => None,
        }
    }

    #[must_use]
    pub fn get_precedence() -> u8 {
        4
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOp {
    Pos,
    Neg,
    Not,
}

impl UnaryOp {
    #[must_use]
    pub fn try_from_token(tok: &Span<Token>) -> Option<Self> {
        match &tok.node {
            Token::Plus => Some(UnaryOp::Pos),
            Token::Minus => Some(UnaryOp::Neg),
            Token::Bang => Some(UnaryOp::Not),
            _ => None,
        }
    }
}
