use std::iter::Peekable;
use thiserror::Error;

use crate::tokenizer::{Token, Tokens};

pub mod ast;

#[derive(Error, Debug, PartialEq)]
pub enum ParseError {
    #[error("unexpected {0:?} where statement was expected")]
    InvalidStmtStart(Option<Token>),
    #[error("unexpected {0:?} where identifier of let statement was expected")]
    InvalidLetIdent(Option<Token>),
    #[error("unexpected {0:?} where '=' of let statement was expected")]
    InvalidLetEq(Option<Token>),
    #[error("unexpected {0:?} where 'then' of if statement was expected")]
    InvalidIfThen(Option<Token>),
    #[error("unexpected {0:?} where expression was expected")]
    InvalidExprStart(Option<Token>),
    #[error("unexpected {0:?} where a closing parenthesis of expression was expected")]
    UnclosedExprParen(Option<Token>),
    #[error("unexpected {0:?} where object key was expected")]
    InvalidObjKey(Option<Token>),
    #[error("unexpected {0:?} where ':' of object property was expected")]
    InvalidObjColon(Option<Token>),
    #[error("unexpected {0:?} where comma or closing brace of object was expected")]
    InvalidObjEnd(Option<Token>),
}

type Result<T> = std::result::Result<T, ParseError>;

pub struct Parser<'a> {
    tokens: Peekable<Tokens<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Parser {
            tokens: Tokens::new(input).peekable(),
        }
    }

    pub fn parse_prog(&mut self) -> Result<ast::Prog> {
        let mut stmts = Vec::new();
        while self.tokens.peek().is_some() {
            stmts.push(self.parse_stmt()?);
        }
        Ok(ast::Prog { stmts })
    }

    fn parse_stmt(&mut self) -> Result<ast::Stmt> {
        let stmt = match self.tokens.peek() {
            Some(Token::CurlyL) => self.parse_block_stmt(),
            Some(Token::Let) => self.parse_let_stmt(),
            Some(Token::Print) => self.parse_print_stmt(),
            Some(Token::If) => self.parse_if_stmt(),
            tok => return Err(ParseError::InvalidStmtStart(tok.cloned())),
        }?;
        // consume semis
        while self.tokens.next_if_eq(&Token::Semi).is_some() {}
        Ok(stmt)
    }

    fn parse_block_stmt(&mut self) -> Result<ast::Stmt> {
        self.tokens.next(); // Consume '{'
        let mut stmts = Vec::new();
        while self.tokens.next_if_eq(&Token::CurlyR).is_none() {
            stmts.push(self.parse_stmt()?);
        }
        Ok(ast::Stmt::Block(stmts))
    }

    fn parse_let_stmt(&mut self) -> Result<ast::Stmt> {
        self.tokens.next(); // Consume 'let'
        let ident = match self.tokens.next() {
            Some(Token::Ident(name)) => name,
            t => return Err(ParseError::InvalidLetIdent(t)),
        };
        match self.tokens.next() {
            Some(Token::Eq) => {}
            t => return Err(ParseError::InvalidLetEq(t)),
        }
        let expr = self.parse_expr(0)?;
        Ok(ast::Stmt::Let { ident, expr })
    }

    fn parse_print_stmt(&mut self) -> Result<ast::Stmt> {
        self.tokens.next(); // Consume 'print'
        let expr = self.parse_expr(0)?;
        Ok(ast::Stmt::Print(expr))
    }

    fn parse_if_stmt(&mut self) -> Result<ast::Stmt> {
        self.tokens.next(); // Consume 'if'
        let condition = self.parse_expr(0)?;

        // Parse then branch
        let then_token = self.tokens.next();
        if then_token != Some(Token::Then) {
            return Err(ParseError::InvalidIfThen(then_token));
        }
        let consequent = Box::new(self.parse_stmt()?);

        // Parse optional else branch
        let alternate = if self.tokens.next_if_eq(&Token::Else).is_some() {
            Some(Box::new(self.parse_stmt()?))
        } else {
            None
        };

        Ok(ast::Stmt::If {
            cond: condition,
            cons: consequent,
            alt: alternate,
        })
    }

    pub fn parse_expr(&mut self, precedence: u8) -> Result<ast::Expr> {
        let mut left = self.parse_operand()?;

        // Handle property access (highest precedence)
        left = self.parse_prop_access(left)?;

        loop {
            let Some(tok) = self.tokens.peek() else {
                break;
            };
            let Some(op) = ast::BinOp::try_from_token(tok) else {
                break;
            };
            let next_prec = op.get_precedence();
            if next_prec <= precedence {
                break;
            }
            self.tokens.next(); // Consume operator
            let right = self.parse_expr(next_prec)?;
            left = ast::Expr::BinOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_operand(&mut self) -> Result<ast::Expr> {
        match self.tokens.next() {
            Some(Token::Num(n)) => Ok(ast::Expr::Num(n)),
            Some(Token::Ident(name)) => Ok(ast::Expr::Var(name)),
            Some(Token::Str(s)) => Ok(ast::Expr::Str(s)),
            Some(Token::ParenL) => {
                let expr = self.parse_expr(0)?;
                let paren = self.tokens.next();
                if paren != Some(Token::ParenR) {
                    return Err(ParseError::UnclosedExprParen(paren));
                }
                Ok(expr)
            }
            Some(Token::CurlyL) => self.parse_obj(),
            t => {
                let op = t.as_ref().and_then(ast::UnaryOp::try_from_token);
                if let Some(op) = op {
                    Ok(ast::Expr::UnaryOp {
                        op,
                        expr: Box::new(self.parse_operand()?),
                    })
                } else {
                    Err(ParseError::InvalidExprStart(t))
                }
            }
        }
    }

    fn parse_obj(&mut self) -> Result<ast::Expr> {
        let mut props = Vec::new();

        while self.tokens.next_if_eq(&Token::CurlyR).is_none() {
            // Parse property key (identifier or string literal)
            let key = match self.tokens.next() {
                Some(Token::Ident(name)) => name,
                Some(Token::Str(s)) => s,
                t => return Err(ParseError::InvalidObjKey(t)),
            };

            // Expect colon
            match self.tokens.next() {
                Some(Token::Colon) => {}
                t => return Err(ParseError::InvalidObjColon(t)),
            }

            // Parse property value
            let val = self.parse_expr(0)?;
            props.push(ast::Prop { key, val });

            // Check for comma or closing brace
            match self.tokens.peek() {
                Some(Token::Comma) => {
                    self.tokens.next(); // consume comma
                }
                Some(Token::CurlyR) => {
                    self.tokens.next(); // consume closing brace
                    break;
                }
                t => return Err(ParseError::InvalidObjEnd(t.cloned())),
            }
        }

        Ok(ast::Expr::Obj { props })
    }

    fn parse_prop_access(&mut self, mut obj: ast::Expr) -> Result<ast::Expr> {
        while self.tokens.next_if_eq(&Token::Dot).is_some() {
            let prop = match self.tokens.next() {
                Some(Token::Ident(name)) => name,
                t => return Err(ParseError::InvalidExprStart(t)),
            };

            obj = ast::Expr::PropAccess {
                obj: Box::new(obj),
                prop,
            };
        }

        Ok(obj)
    }
}

#[cfg(test)]
mod tests;
