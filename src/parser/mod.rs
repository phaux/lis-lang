use std::iter::Peekable;
use thiserror::Error;

use crate::{
    parser::ast::{BinOp, Expr, Prog, Prop, Stmt, UnaryOp},
    tokenizer::{Token, Tokens},
};

pub mod ast;

#[derive(Error, Debug, PartialEq)]
pub enum ParseError {
    #[error("unexpected {0:?} where statement was expected")]
    StmtInvalidStart(Option<Token>),
    #[error("unexpected {0:?} where identifier of let statement was expected")]
    LetExpectedIdent(Option<Token>),
    #[error("unexpected {0:?} where '=' of let statement was expected")]
    LetExpectedEq(Option<Token>),
    #[error("unexpected {0:?} where 'then' of if statement was expected")]
    IfExpectedThen(Option<Token>),
    #[error("unexpected {0:?} where expression was expected")]
    ExprInvalidStart(Option<Token>),
    #[error("unexpected {0:?} where a closing parenthesis of expression was expected")]
    ExprUnclosedParen(Option<Token>),
    #[error("unexpected {0:?} where object key was expected")]
    ObjExpectedKey(Option<Token>),
    #[error("unexpected {0:?} where ':' of object property was expected")]
    ObjExpectedColon(Option<Token>),
    #[error("unexpected {0:?} where comma or closing brace of object was expected")]
    PropInvalidEnd(Option<Token>),
    #[error("unexpected {0:?} where a function name was expected")]
    FnExpectedName(Option<Token>),
    #[error("unexpected {0:?} where a function parenthesis was expected")]
    FnExpectedParen(Option<Token>),
    #[error("unexpected {0:?} where a function body was expected")]
    FnExpectedBody(Option<Token>),
    #[error("unexpected {0:?} where a function parameter name was expected")]
    ParamExpectedIdent(Option<Token>),
    #[error(
        "unexpected {0:?} where a comma or closing parenthesis of function parameters was expected"
    )]
    ParamInvalidEnd(Option<Token>),
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

    pub fn parse_prog(&mut self) -> Result<Prog> {
        let mut stmts = Vec::new();
        while self.tokens.peek().is_some() {
            stmts.push(self.parse_stmt()?);
        }
        Ok(Prog { stmts })
    }

    fn parse_stmt(&mut self) -> Result<Stmt> {
        let stmt = match self.tokens.peek() {
            Some(Token::Semi) => Ok(Stmt::Noop),
            Some(Token::CurlyL) => self.parse_block_stmt(),
            Some(Token::Let) => self.parse_let_stmt(),
            Some(Token::Fn) => self.parse_func_decl(),
            Some(Token::Print) => self.parse_print_stmt(),
            Some(Token::If) => self.parse_if_stmt(),
            Some(_) => Ok(Stmt::Expr(self.parse_expr(0)?)),
            None => return Err(ParseError::StmtInvalidStart(None)),
        }?;

        // Consume semi after the statement
        self.tokens.next_if_eq(&Token::Semi);

        Ok(stmt)
    }

    fn parse_block_stmt(&mut self) -> Result<Stmt> {
        self.tokens.next(); // Consume '{'
        let mut stmts = Vec::new();
        while self.tokens.next_if_eq(&Token::CurlyR).is_none() {
            stmts.push(self.parse_stmt()?);
        }
        Ok(Stmt::Block(stmts))
    }

    fn parse_let_stmt(&mut self) -> Result<Stmt> {
        self.tokens.next(); // Consume 'let'
        let ident = match self.tokens.next() {
            Some(Token::Ident(name)) => name,
            t => return Err(ParseError::LetExpectedIdent(t)),
        };
        match self.tokens.next() {
            Some(Token::Eq) => {}
            t => return Err(ParseError::LetExpectedEq(t)),
        }
        let expr = self.parse_expr(0)?;
        Ok(Stmt::Let { ident, expr })
    }

    fn parse_func_decl(&mut self) -> Result<Stmt> {
        // Consume 'fn' token
        self.tokens.next();

        // Parse function name
        let name = match self.tokens.next() {
            Some(Token::Ident(name)) => name,
            token => return Err(ParseError::FnExpectedName(token)),
        };

        self.tokens
            .next_if_eq(&Token::ParenL)
            .ok_or(ParseError::FnExpectedParen(None))?;

        // Parse parameters
        let mut params = Vec::new();
        while self.tokens.next_if_eq(&Token::ParenR).is_none() {
            let Token::Ident(param) = self.tokens.next().unwrap() else {
                return Err(ParseError::ParamExpectedIdent(None));
            };
            params.push(param);

            match self.tokens.peek() {
                Some(Token::Comma) => {
                    self.tokens.next();
                }
                Some(Token::ParenR) => {}
                _ => return Err(ParseError::ParamInvalidEnd(self.tokens.next())),
            }
        }

        // Parse function body (must be a block statement)
        let body = match self.tokens.peek() {
            Some(Token::CurlyL) => self.parse_block_stmt()?,
            _ => return Err(ParseError::FnExpectedBody(self.tokens.next())),
        };

        Ok(Stmt::FuncDecl(ast::FuncDecl {
            name,
            params,
            body: Box::new(body),
        }))
    }

    fn parse_print_stmt(&mut self) -> Result<Stmt> {
        self.tokens.next(); // Consume 'print'
        let expr = self.parse_expr(0)?;
        Ok(Stmt::Print(expr))
    }

    fn parse_if_stmt(&mut self) -> Result<Stmt> {
        self.tokens.next(); // Consume 'if'
        let condition = self.parse_expr(0)?;

        // Parse then branch
        let then_token = self.tokens.next();
        if then_token != Some(Token::Then) {
            return Err(ParseError::IfExpectedThen(then_token));
        }
        let consequent = Box::new(self.parse_stmt()?);

        // Parse optional else branch
        let alternate = if self.tokens.next_if_eq(&Token::Else).is_some() {
            Some(Box::new(self.parse_stmt()?))
        } else {
            None
        };

        Ok(Stmt::If {
            cond: condition,
            cons: consequent,
            alt: alternate,
        })
    }

    pub fn parse_expr(&mut self, precedence: u8) -> Result<Expr> {
        let mut left = self.parse_operand()?;

        // Handle property access (highest precedence)
        left = self.parse_prop_access(left)?;

        // Handle assignment expression (right associative)
        if let Some(Token::Eq) = self.tokens.peek() {
            self.tokens.next(); // Consume '='
            let right = self.parse_expr(0)?;
            left = Expr::Assign {
                place: Box::new(left),
                expr: Box::new(right),
            };
        }

        // Handle binary operations
        loop {
            let Some(tok) = self.tokens.peek() else {
                break;
            };
            let Some(op) = BinOp::try_from_token(tok) else {
                break;
            };
            let next_prec = op.get_precedence();
            if next_prec <= precedence {
                break;
            }
            self.tokens.next(); // Consume operator
            let right = self.parse_expr(next_prec)?;
            left = Expr::BinOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_operand(&mut self) -> Result<Expr> {
        match self.tokens.next() {
            Some(Token::Num(n)) => Ok(Expr::Num(n)),
            Some(Token::Ident(name)) => Ok(Expr::Var(name)),
            Some(Token::Str(s)) => Ok(Expr::Str(s)),
            Some(Token::ParenL) => {
                let expr = self.parse_expr(0)?;
                let paren = self.tokens.next();
                if paren != Some(Token::ParenR) {
                    return Err(ParseError::ExprUnclosedParen(paren));
                }
                Ok(expr)
            }
            Some(Token::CurlyL) => self.parse_obj(),
            t => {
                let op = t.as_ref().and_then(UnaryOp::try_from_token);
                if let Some(op) = op {
                    Ok(Expr::UnaryOp {
                        op,
                        expr: Box::new(self.parse_operand()?),
                    })
                } else {
                    Err(ParseError::ExprInvalidStart(t))
                }
            }
        }
    }

    fn parse_obj(&mut self) -> Result<Expr> {
        let mut props = Vec::new();

        while self.tokens.next_if_eq(&Token::CurlyR).is_none() {
            // Parse property key (identifier or string literal)
            let key = match self.tokens.next() {
                Some(Token::Ident(name)) => name,
                Some(Token::Str(s)) => s,
                t => return Err(ParseError::ObjExpectedKey(t)),
            };

            // Expect colon
            match self.tokens.next() {
                Some(Token::Colon) => {}
                t => return Err(ParseError::ObjExpectedColon(t)),
            }

            // Parse property value
            let val = self.parse_expr(0)?;
            props.push(Prop { key, val });

            // Check for comma or closing brace
            match self.tokens.peek() {
                Some(Token::Comma) => {
                    self.tokens.next(); // consume comma
                }
                Some(Token::CurlyR) => {
                    self.tokens.next(); // consume closing brace
                    break;
                }
                t => return Err(ParseError::PropInvalidEnd(t.cloned())),
            }
        }

        Ok(Expr::Obj { props })
    }

    fn parse_prop_access(&mut self, mut obj: Expr) -> Result<Expr> {
        while self.tokens.next_if_eq(&Token::Dot).is_some() {
            let prop = match self.tokens.next() {
                Some(Token::Ident(name)) => name,
                t => return Err(ParseError::ExprInvalidStart(t)),
            };

            obj = Expr::PropAccess {
                obj: Box::new(obj),
                prop,
            };
        }

        Ok(obj)
    }
}

#[cfg(test)]
mod tests;
