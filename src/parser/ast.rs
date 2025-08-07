use crate::tokenizer::Token;

#[derive(Debug, PartialEq, Clone)]
pub struct Prog {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Let {
        ident: String,
        expr: Expr,
    },
    Print(Expr),
    Block(Vec<Stmt>),
    If {
        cond: Expr,
        cons: Box<Stmt>,
        alt: Option<Box<Stmt>>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Num(f64),
    Var(String),
    Str(String),
    BinOp {
        left: Box<Expr>,
        op: BinOp,
        right: Box<Expr>,
    },
    UnaryOp {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Obj {
        props: Vec<Prop>,
    },
    PropAccess {
        obj: Box<Expr>,
        prop: String,
    },
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinOp {
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

#[derive(Debug, PartialEq, Clone)]
pub struct Prop {
    pub key: String,
    pub val: Expr,
}

impl UnaryOp {
    pub fn try_from_token(token: &Token) -> Option<Self> {
        match token {
            Token::Plus => Some(UnaryOp::Pos),
            Token::Minus => Some(UnaryOp::Neg),
            Token::Bang => Some(UnaryOp::Not),
            _ => None,
        }
    }
}

impl BinOp {
    pub fn try_from_token(token: &Token) -> Option<Self> {
        match token {
            Token::Plus => Some(BinOp::Add),
            Token::PlusPlus => Some(BinOp::Concat),
            Token::Minus => Some(BinOp::Sub),
            Token::Star => Some(BinOp::Mul),
            Token::Slash => Some(BinOp::Div),
            Token::EqEq => Some(BinOp::Eq),
            Token::BangEq => Some(BinOp::NotEq),
            _ => None,
        }
    }

    pub fn get_precedence(self) -> u8 {
        match self {
            // BinOp::Or => 1,
            // BinOp::And => 2,
            BinOp::Eq | BinOp::NotEq => 3,
            BinOp::Concat => 4,
            BinOp::Add | BinOp::Sub => 5,
            BinOp::Mul | BinOp::Div => 6,
            // BinOp::Mod => 7,
            // BinOp::Pow => 8,
            // BinOp::Default => 9,
        }
    }
}
