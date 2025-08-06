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
        condition: Expr,
        consequent: Box<Stmt>,
        alternate: Option<Box<Stmt>>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Number(f64),
    Ident(String),
    BinOp {
        left: Box<Expr>,
        op: BinOp,
        right: Box<Expr>,
    },
    UnaryOp {
        op: UnaryOp,
        expr: Box<Expr>,
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
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOp {
    Pos,
    Neg,
    Not,
}

impl TryFrom<&Token> for UnaryOp {
    type Error = Token;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        match token {
            Token::Plus => Ok(UnaryOp::Pos),
            Token::Minus => Ok(UnaryOp::Neg),
            Token::Bang => Ok(UnaryOp::Not),
            _ => Err(token.clone()),
        }
    }
}

impl BinOp {
    pub fn get_precedence(self) -> u8 {
        match self {
            BinOp::Eq | BinOp::NotEq => 1,
            BinOp::Add | BinOp::Sub => 2,
            BinOp::Mul | BinOp::Div => 3,
        }
    }
}

impl TryFrom<&Token> for BinOp {
    type Error = Token;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        match token {
            Token::Plus => Ok(BinOp::Add),
            Token::Minus => Ok(BinOp::Sub),
            Token::Star => Ok(BinOp::Mul),
            Token::Slash => Ok(BinOp::Div),
            Token::EqEq => Ok(BinOp::Eq),
            Token::BangEq => Ok(BinOp::NotEq),
            _ => Err(token.clone()),
        }
    }
}
