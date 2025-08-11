use ::std::iter::Peekable;

use thiserror::Error;

use crate::{
    ast::{BinOp, Expr, FuncDecl, Pat, Prog, Stmt, UnaryOp},
    tokenizer::{Keyword, Token, Tokens},
};

#[derive(Error, Debug, PartialEq)]
pub enum ParseError {
    #[error("unexpected {0:?} where statement was expected")]
    StmtInvalidStart(Option<Token>),
    #[error("unexpected {0:?} where identifier of let statement was expected")]
    PatExpectedIdent(Option<Token>),
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
    ObjInvalidEnd(Option<Token>),
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
    ParamsInvalidEnd(Option<Token>),
    #[error(
        "unexpected {0:?} where a comma or closing parenthesis of function arguments was expected"
    )]
    ArgsInvalidEnd(Option<Token>),
}

type Result<T> = std::result::Result<T, ParseError>;

pub struct Parser<'a> {
    tokens: Peekable<Tokens<'a>>,
}

impl<'a> Parser<'a> {
    #[must_use]
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
            Some(Token::Keyword(Keyword::Let)) => self.parse_let_stmt(),
            Some(Token::Keyword(Keyword::Fn)) => self.parse_func_decl(),
            Some(Token::Keyword(Keyword::Print)) => self.parse_print_stmt(),
            Some(Token::Keyword(Keyword::If)) => self.parse_if_stmt(),
            Some(Token::Keyword(Keyword::Return)) => self.parse_return_stmt(),
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

        // Parse the pattern
        let pattern = self.parse_pattern()?;

        // Expect '=' after pattern in let statement
        match self.tokens.next() {
            Some(Token::Eq) => {}
            t => return Err(ParseError::LetExpectedEq(t)),
        }

        // Parse expression
        let expr = self.parse_expr(0)?;

        Ok(Stmt::Let { pat: pattern, expr })
    }

    fn parse_pattern(&mut self) -> Result<Pat> {
        match self.tokens.next() {
            Some(Token::Ident(name)) => Ok(Pat::Ident(name)),
            Some(Token::CurlyL) => self.parse_obj_pat(),
            t => Err(ParseError::PatExpectedIdent(t)),
        }
    }

    fn parse_obj_pat(&mut self) -> Result<Pat> {
        let mut props = Vec::new();
        while self.tokens.next_if_eq(&Token::CurlyR).is_none() {
            // Parse property name
            let key = match self.tokens.next() {
                Some(Token::Ident(name)) => name,
                t => return Err(ParseError::ObjExpectedKey(t)),
            };

            // Check for nested pattern
            let mut pat = if self.tokens.next_if_eq(&Token::Colon).is_some() {
                self.parse_pattern()?
            } else {
                Pat::Ident(key.clone())
            };

            // Check for default value after pattern
            if self.tokens.next_if_eq(&Token::Eq).is_some() {
                let default = self.parse_expr(0)?;
                pat = Pat::Default {
                    pat: Box::new(pat),
                    default,
                };
            }

            props.push((key, pat));

            // Check for comma or closing brace
            match self.tokens.peek() {
                Some(Token::Comma) => {
                    self.tokens.next(); // consume comma
                }
                Some(Token::CurlyR) => {}
                t => return Err(ParseError::ObjInvalidEnd(t.cloned())),
            }
        }
        Ok(Pat::Obj { props })
    }

    fn parse_func_decl(&mut self) -> Result<Stmt> {
        // Consume 'fn'
        self.tokens.next();

        // Parse function name
        let name = match self.tokens.next() {
            Some(Token::Ident(name)) => name,
            token => return Err(ParseError::FnExpectedName(token)),
        };

        // Expect '('
        match self.tokens.next() {
            Some(Token::ParenL) => {}
            t => return Err(ParseError::FnExpectedParen(t)),
        }

        // Parse parameters
        let mut params = Vec::new();
        while self.tokens.next_if_eq(&Token::ParenR).is_none() {
            // Parse parameter name
            match self.tokens.next() {
                Some(Token::Ident(param)) => params.push(param),
                t => return Err(ParseError::ParamExpectedIdent(t)),
            }
            // Expect ',' or ')'
            match self.tokens.peek() {
                Some(Token::Comma) => {
                    self.tokens.next();
                }
                Some(Token::ParenR) => {}
                _ => return Err(ParseError::ParamsInvalidEnd(self.tokens.next())),
            }
        }

        // Parse function body (must be a block statement)
        let body = match self.tokens.peek() {
            Some(Token::CurlyL) => self.parse_block_stmt()?,
            t => return Err(ParseError::FnExpectedBody(t.cloned())),
        };

        Ok(Stmt::FuncDecl(FuncDecl {
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

    fn parse_return_stmt(&mut self) -> Result<Stmt> {
        self.tokens.next(); // Consume 'return'
        match self.tokens.peek() {
            Some(Token::Semi | Token::CurlyR) | None => Ok(Stmt::Return(None)),
            _ => Ok(Stmt::Return(Some(self.parse_expr(0)?))),
        }
    }

    fn parse_if_stmt(&mut self) -> Result<Stmt> {
        self.tokens.next(); // Consume 'if'

        // Parse condition
        let condition = self.parse_expr(0)?;

        // Expect 'then'
        match self.tokens.next() {
            Some(Token::Keyword(Keyword::Then)) => {}
            t => return Err(ParseError::IfExpectedThen(t)),
        }

        // Parse consequent branch
        let consequent = Box::new(self.parse_stmt()?);

        // Parse optional alternate branch
        let alternate = match self.tokens.next_if_eq(&Token::Keyword(Keyword::Else)) {
            Some(_) => Some(Box::new(self.parse_stmt()?)),
            None => None,
        };

        Ok(Stmt::If {
            cond: condition,
            cons: consequent,
            alt: alternate,
        })
    }

    pub fn parse_expr(&mut self, precedence: u8) -> Result<Expr> {
        // Parse left operand
        let mut left = self.parse_operand()?;

        // Handle property access and function calls (highest precedence)
        loop {
            left = match self.tokens.peek() {
                Some(&Token::Dot) => self.parse_prop_access(left)?,
                Some(&Token::ParenL) => Expr::FuncCall {
                    func: Box::new(left),
                    args: self.parse_func_call()?,
                },
                _ => break,
            };
        }

        // Handle assignment expression (right associative)
        if self.tokens.next_if_eq(&Token::Eq).is_some() {
            left = Expr::Assign {
                place: Box::new(left),
                expr: Box::new(self.parse_expr(0)?),
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
            left = Expr::BinOp {
                left: Box::new(left),
                op,
                right: Box::new(self.parse_expr(next_prec)?),
            };
        }

        Ok(left)
    }

    fn parse_operand(&mut self) -> Result<Expr> {
        match self.tokens.next() {
            Some(Token::Keyword(Keyword::Nil)) => Ok(Expr::Nil),
            Some(Token::Num(n)) => Ok(Expr::Num(n)),
            Some(Token::Str(s)) => Ok(Expr::Str(s)),
            Some(Token::Ident(name)) => Ok(Expr::Var(name)),
            Some(Token::ParenL) => {
                // Parse parenthesized expression
                let expr = self.parse_expr(0)?;
                match self.tokens.next() {
                    Some(Token::ParenR) => {}
                    t => return Err(ParseError::ExprUnclosedParen(t)),
                }
                Ok(expr)
            }
            Some(Token::CurlyL) => self.parse_obj(),
            t => {
                let op = t.as_ref().and_then(UnaryOp::try_from_token);
                match op {
                    Some(op) => {
                        // Parse unary operator
                        let expr = Box::new(self.parse_operand()?);
                        Ok(Expr::UnaryOp { op, expr })
                    }
                    None => Err(ParseError::ExprInvalidStart(t)),
                }
            }
        }
    }

    fn parse_prop_access(&mut self, obj: Expr) -> Result<Expr> {
        self.tokens.next(); // Consume '.'

        // Parse property name
        let prop = match self.tokens.next() {
            Some(Token::Ident(name)) => name,
            t => return Err(ParseError::ExprInvalidStart(t)),
        };

        Ok(Expr::PropAccess {
            obj: Box::new(obj),
            prop,
        })
    }

    fn parse_func_call(&mut self) -> Result<Vec<Expr>> {
        self.tokens.next(); // Consume '('

        // Parse arguments
        let mut args = Vec::new();
        while self.tokens.next_if_eq(&Token::ParenR).is_none() {
            // Parse argument
            args.push(self.parse_expr(0)?);

            // Check for comma or closing parenthesis
            match self.tokens.peek() {
                Some(Token::Comma) => {
                    self.tokens.next(); // Consume ','
                }
                Some(Token::ParenR) => {}
                t => return Err(ParseError::ArgsInvalidEnd(t.cloned())),
            }
        }

        Ok(args)
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
            props.push((key, val));

            // Check for comma or closing brace
            match self.tokens.peek() {
                Some(Token::Comma) => {
                    self.tokens.next(); // consume comma
                }
                Some(Token::CurlyR) => {}
                t => return Err(ParseError::ObjInvalidEnd(t.cloned())),
            }
        }

        Ok(Expr::Obj { props })
    }
}

#[cfg(test)]
#[path = "parser.tests.rs"]
mod tests;
