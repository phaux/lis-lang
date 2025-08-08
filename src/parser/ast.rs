use crate::tokenizer::Token;

#[derive(Debug, PartialEq, Clone)]
pub struct Prog {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Noop,
    Expr(Expr),
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
    Return(Option<Expr>),
    FuncDecl(FuncDecl),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FuncDecl {
    pub name: String,
    pub params: Vec<String>,
    pub body: Box<Stmt>,
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
    Assign {
        place: Box<Expr>,
        expr: Box<Expr>,
    },
    Obj {
        props: Vec<Prop>,
    },
    PropAccess {
        obj: Box<Expr>,
        prop: String,
    },
    FuncCall {
        func: Box<Expr>,
        args: Vec<Expr>,
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
            // BinOp::Or => 2,
            // BinOp::And => 3,
            BinOp::Eq | BinOp::NotEq => 4,
            BinOp::Concat => 5,
            BinOp::Add | BinOp::Sub => 6,
            BinOp::Mul | BinOp::Div => 7,
            // BinOp::Mod => 8,
            // BinOp::Pow => 9,
            // BinOp::Default => 10,
        }
    }
}
