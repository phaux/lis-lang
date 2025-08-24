//! This module defines types which represent abstract syntax tree nodes.
//! The root of the AST is a [Prog].

use std::ops::Range;

use crate::token::{Keyword, Pos, Sigil, Token};

/// AST Node representing a whole program.
#[derive(Debug, PartialEq, Clone)]
pub struct Prog {
    pub stmts: Vec<Span<Stmt>>,
}

/// A wrapper type which adds information about node's position in source code.
#[derive(Debug, PartialEq, Clone)]
pub struct Span<T> {
    pub offset: Range<usize>,
    pub pos: Range<Pos>,
    pub node: T,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Expr {
        expr: Box<Span<Expr>>,
    },
    Let {
        keyword: Token,
        pat: Box<Span<Pat>>,
        eq_tok: Token,
        init: Box<Span<Expr>>,
    },
    Print {
        keyword: Token,
        expr: Box<Span<Expr>>,
    },
    Block {
        brace_l: Token,
        stmts: Vec<Span<Stmt>>,
        brace_r: Token,
    },
    If {
        keyword: Token,
        condition: Box<Span<Expr>>,
        then_keyword: Token,
        cons_branch: Box<Span<Stmt>>,
        else_keyword: Option<Token>,
        alt_branch: Option<Box<Span<Stmt>>>,
    },
    Return {
        keyword: Token,
        expr: Option<Box<Span<Expr>>>,
    },
    FuncDecl {
        keyword: Token,
        name: String,
        param_paren_l: Token,
        params: Vec<String>,
        param_paren_r: Token,
        body: Box<Span<Stmt>>,
    },
    While {
        keyword: Token,
        condition: Box<Span<Expr>>,
        do_keyword: Token,
        body: Box<Span<Stmt>>,
    },
    Break {
        keyword: Token,
    },
    Continue {
        keyword: Token,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pat {
    Ident {
        name: String,
    },
    Obj {
        brace_l: Token,
        props: Vec<(String, Span<Pat>)>,
        brace_r: Token,
    },
    Default {
        pat: Box<Span<Pat>>,
        eq_tok: Token,
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
    BinOp {
        left: Box<Span<Expr>>,
        op: BinOp,
        op_tok: Token,
        right: Box<Span<Expr>>,
    },
    UnaryOp {
        op: UnaryOp,
        op_tok: Token,
        expr: Box<Span<Expr>>,
    },
    Assign {
        place: Box<Span<Expr>>,
        eq_tok: Token,
        expr: Box<Span<Expr>>,
    },
    Obj {
        brace_l: Token,
        props: Vec<(String, Span<Expr>)>,
        brace_r: Token,
    },
    PropAccess {
        obj: Box<Span<Expr>>,
        dot_tok: Token,
        prop: String,
    },
    FuncCall {
        callee: Box<Span<Expr>>,
        paren_l: Token,
        args: Vec<Span<Expr>>,
        paren_r: Token,
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
    Eq,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Concat,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOp {
    Pos,
    Neg,
    Not,
}

impl UnaryOp {
    #[must_use]
    pub fn try_from_token(tok: &Token) -> Option<Self> {
        match &tok.sigil {
            Sigil::Plus => Some(UnaryOp::Pos),
            Sigil::Minus => Some(UnaryOp::Neg),
            Sigil::Bang => Some(UnaryOp::Not),
            _ => None,
        }
    }
}

impl BinOp {
    #[must_use]
    pub fn try_from_token(tok: &Token) -> Option<Self> {
        match &tok.sigil {
            Sigil::Keyword(Keyword::Or) => Some(BinOp::Or),
            Sigil::Keyword(Keyword::And) => Some(BinOp::And),
            Sigil::Plus => Some(BinOp::Add),
            Sigil::PlusPlus => Some(BinOp::Concat),
            Sigil::Minus => Some(BinOp::Sub),
            Sigil::Star => Some(BinOp::Mul),
            Sigil::Slash => Some(BinOp::Div),
            Sigil::EqEq => Some(BinOp::Eq),
            Sigil::BangEq => Some(BinOp::NotEq),
            Sigil::Less => Some(BinOp::Less),
            Sigil::LessEq => Some(BinOp::LessEq),
            Sigil::Greater => Some(BinOp::Greater),
            Sigil::GreaterEq => Some(BinOp::GreaterEq),
            _ => None,
        }
    }

    #[must_use]
    pub fn get_precedence(self) -> u8 {
        match self {
            BinOp::Or => 2,
            BinOp::And => 3,
            BinOp::Eq
            | BinOp::NotEq
            | BinOp::Less
            | BinOp::LessEq
            | BinOp::Greater
            | BinOp::GreaterEq => 4,
            BinOp::Concat => 5,
            BinOp::Add | BinOp::Sub => 6,
            BinOp::Mul | BinOp::Div => 7,
        }
    }
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
