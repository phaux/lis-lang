use std::ops::Range;

use crate::lexer::{Keyword, Sigil, Token};

#[derive(Debug, PartialEq, Clone)]
pub struct Prog {
    pub stmts: Vec<Node<Stmt>>,
}

/// A wrapper type which adds properties common to all AST nodes.
#[derive(Debug, PartialEq, Clone)]
pub struct Node<T> {
    pub range: Range<usize>,
    pub node: T,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Expr {
        expr: Node<Expr>,
    },
    Let {
        keyword: Token,
        pat: Node<Pat>,
        eq_tok: Token,
        init: Node<Expr>,
    },
    Print {
        keyword: Token,
        expr: Node<Expr>,
    },
    Block {
        brace_l: Token,
        stmts: Vec<Node<Stmt>>,
        brace_r: Token,
    },
    If {
        keyword: Token,
        condition: Node<Expr>,
        then_keyword: Token,
        cons_branch: Box<Node<Stmt>>,
        else_keyword: Option<Token>,
        alt_branch: Option<Box<Node<Stmt>>>,
    },
    Return {
        keyword: Token,
        expr: Option<Node<Expr>>,
    },
    FuncDecl {
        keyword: Token,
        name: String,
        param_paren_l: Token,
        params: Vec<String>,
        param_paren_r: Token,
        body: Box<Node<Stmt>>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pat {
    Ident {
        name: String,
    },
    Obj {
        brace_l: Token,
        props: Vec<(String, Node<Pat>)>,
        brace_r: Token,
    },
    Default {
        pat: Box<Node<Pat>>,
        eq_tok: Token,
        default: Node<Expr>,
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
        left: Box<Node<Expr>>,
        op: BinOp,
        op_tok: Token,
        right: Box<Node<Expr>>,
    },
    UnaryOp {
        op: UnaryOp,
        op_tok: Token,
        expr: Box<Node<Expr>>,
    },
    Assign {
        place: Box<Node<Expr>>,
        eq_tok: Token,
        expr: Box<Node<Expr>>,
    },
    Obj {
        brace_l: Token,
        props: Vec<(String, Node<Expr>)>,
        brace_r: Token,
    },
    PropAccess {
        obj: Box<Node<Expr>>,
        dot_tok: Token,
        prop: String,
    },
    FuncCall {
        callee: Box<Node<Expr>>,
        paren_l: Token,
        args: Vec<Node<Expr>>,
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
            _ => None,
        }
    }

    #[must_use]
    pub fn get_precedence(self) -> u8 {
        match self {
            BinOp::Or => 2,
            BinOp::And => 3,
            BinOp::Eq | BinOp::NotEq => 4,
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
            Stmt::Expr { .. } | Stmt::Let { .. } | Stmt::Print { .. } | Stmt::Return { .. } => true,
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
        }
    }
}
