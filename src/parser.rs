use std::iter::Peekable;
use thiserror::Error;

use crate::ast;
use crate::tokenizer::{Token, Tokenizer};

#[derive(Error, Debug, PartialEq)]
pub enum ParseError {
    #[error("unexpected {0:?} where statement was expected")]
    InvalidStmt(Option<Token>),
    #[error("unexpected {0:?} where identifier of let statement was expected")]
    InvalidLetIdent(Option<Token>),
    #[error("unexpected {0:?} where '=' of let statement was expected")]
    InvalidLetEq(Option<Token>),
    #[error("unexpected {0:?} where 'then' of if statement was expected")]
    InvalidIfThen(Option<Token>),
    #[error("unexpected {0:?} where expression was expected")]
    InvalidExpr(Option<Token>),
    #[error("unexpected {0:?} where a closing parenthesis of expression was expected")]
    UnclosedExprParen(Option<Token>),
}

type Result<T> = std::result::Result<T, ParseError>;

pub struct Parser<'a> {
    tokenizer: Peekable<Tokenizer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Parser {
            tokenizer: Tokenizer::new(input).peekable(),
        }
    }

    pub fn parse_prog(&mut self) -> Result<ast::Prog> {
        let mut stmts = Vec::new();
        while self.tokenizer.peek().is_some() {
            stmts.push(self.parse_stmt()?);
        }
        Ok(ast::Prog { stmts })
    }

    fn parse_stmt(&mut self) -> Result<ast::Stmt> {
        let stmt = match self.tokenizer.peek() {
            Some(Token::CurlyL) => self.parse_block_stmt(),
            Some(Token::Let) => self.parse_let_stmt(),
            Some(Token::Print) => self.parse_print_stmt(),
            Some(Token::If) => self.parse_if_stmt(),
            tok => return Err(ParseError::InvalidStmt(tok.cloned())),
        }?;
        // consume semis
        while self.tokenizer.next_if_eq(&Token::Semi).is_some() {}
        Ok(stmt)
    }

    fn parse_block_stmt(&mut self) -> Result<ast::Stmt> {
        self.tokenizer.next(); // Consume '{'
        let mut stmts = Vec::new();
        while self.tokenizer.next_if_eq(&Token::CurlyR).is_none() {
            stmts.push(self.parse_stmt()?)
        }
        Ok(ast::Stmt::Block(stmts))
    }

    fn parse_let_stmt(&mut self) -> Result<ast::Stmt> {
        self.tokenizer.next(); // Consume 'let'
        let ident = match self.tokenizer.next() {
            Some(Token::Ident(name)) => name,
            t => return Err(ParseError::InvalidLetIdent(t)),
        };
        match self.tokenizer.next() {
            Some(Token::Eq) => {}
            t => return Err(ParseError::InvalidLetEq(t)),
        };
        let expr = self.parse_expr(0)?;
        Ok(ast::Stmt::Let { ident, expr })
    }

    fn parse_print_stmt(&mut self) -> Result<ast::Stmt> {
        self.tokenizer.next(); // Consume 'print'
        let expr = self.parse_expr(0)?;
        Ok(ast::Stmt::Print(expr))
    }

    fn parse_if_stmt(&mut self) -> Result<ast::Stmt> {
        self.tokenizer.next(); // Consume 'if'
        let condition = self.parse_expr(0)?;

        // Parse then branch
        let then_token = self.tokenizer.next();
        if then_token != Some(Token::Then) {
            return Err(ParseError::InvalidIfThen(then_token));
        }
        let consequent = Box::new(self.parse_stmt()?);

        // Parse optional else branch
        let alternate = if self.tokenizer.next_if_eq(&Token::Else).is_some() {
            Some(Box::new(self.parse_stmt()?))
        } else {
            None
        };

        Ok(ast::Stmt::If {
            condition,
            consequent,
            alternate,
        })
    }

    pub fn parse_expr(&mut self, precedence: u8) -> Result<ast::Expr> {
        let mut left = self.parse_operand()?;
        loop {
            let Some(tok) = self.tokenizer.peek() else {
                break;
            };
            let Ok(op) = ast::BinOp::try_from(tok) else {
                break;
            };
            let next_prec = op.get_precedence();
            if next_prec <= precedence {
                break;
            }
            self.tokenizer.next(); // Consume operator
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
        match self.tokenizer.next() {
            Some(Token::Num(n)) => Ok(ast::Expr::Number(n)),
            Some(Token::Ident(name)) => Ok(ast::Expr::Ident(name)),
            Some(Token::ParenL) => {
                let expr = self.parse_expr(0)?;
                let paren = self.tokenizer.next();
                if paren != Some(Token::ParenR) {
                    return Err(ParseError::UnclosedExprParen(paren));
                }
                Ok(expr)
            }
            t => {
                let op = t.as_ref().and_then(|t| ast::UnaryOp::try_from(t).ok());
                if let Some(op) = op {
                    Ok(ast::Expr::UnaryOp {
                        op,
                        expr: Box::new(self.parse_operand()?),
                    })
                } else {
                    Err(ParseError::InvalidExpr(t))
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;

    #[test]
    fn empty_prog() -> Result<()> {
        let prog = Parser::new("").parse_prog()?;
        assert_eq!(prog.stmts.len(), 0);
        Ok(())
    }

    #[test]
    fn many_semis_prog() -> Result<()> {
        let prog = Parser::new("print 1;;;;;").parse_prog()?;
        assert_eq!(prog.stmts.len(), 1);
        Ok(())
    }

    #[test]
    fn only_semis_prog() -> Result<()> {
        let prog = Parser::new(";;;;").parse_prog();
        assert_eq!(prog, Err(ParseError::InvalidStmt(Some(Token::Semi))));
        Ok(())
    }

    #[test]
    fn unclosed_paren() -> Result<()> {
        let prog = Parser::new("(1 + 2;").parse_expr(0);
        assert_eq!(prog, Err(ParseError::UnclosedExprParen(Some(Token::Semi))));
        Ok(())
    }

    #[test]
    fn assignment_prog() -> Result<()> {
        let prog = Parser::new("let x = 1;").parse_prog()?;
        assert_eq!(
            prog,
            Prog {
                stmts: vec![Stmt::Let {
                    ident: "x".to_string(),
                    expr: Expr::Number(1.0),
                }],
            },
        );
        Ok(())
    }

    #[test]
    fn print_stmt() -> Result<()> {
        let stmt = Parser::new("print 1").parse_stmt()?;
        assert_eq!(stmt, Stmt::Print(Expr::Number(1.0)));
        Ok(())
    }

    #[test]
    fn unary_ops() -> Result<()> {
        let expr = Parser::new("+-1").parse_expr(0)?;
        assert_eq!(
            expr,
            Expr::UnaryOp {
                op: UnaryOp::Pos,
                expr: Box::new(Expr::UnaryOp {
                    op: UnaryOp::Neg,
                    expr: Box::new(Expr::Number(1.0))
                })
            }
        );
        Ok(())
    }

    #[test]
    fn binary_ops() -> Result<()> {
        let expr = Parser::new("1 + 2 * 3").parse_expr(0)?;
        assert_eq!(
            expr,
            Expr::BinOp {
                left: Box::new(Expr::Number(1.0)),
                op: BinOp::Add,
                right: Box::new(Expr::BinOp {
                    left: Box::new(Expr::Number(2.0)),
                    op: BinOp::Mul,
                    right: Box::new(Expr::Number(3.0)),
                }),
            }
        );
        Ok(())
    }

    #[test]
    fn binary_and_unary_ops() -> Result<()> {
        let expr = Parser::new("1 + -2 * 3").parse_expr(0)?;
        assert_eq!(
            expr,
            Expr::BinOp {
                left: Box::new(Expr::Number(1.0)),
                op: BinOp::Add,
                right: Box::new(Expr::BinOp {
                    left: Box::new(Expr::UnaryOp {
                        op: UnaryOp::Neg,
                        expr: Box::new(Expr::Number(2.0)),
                    }),
                    op: BinOp::Mul,
                    right: Box::new(Expr::Number(3.0)),
                }),
            }
        );
        Ok(())
    }

    #[test]
    fn parens() -> Result<()> {
        let expr = Parser::new("(1 + 2) * 3").parse_expr(0)?;
        assert_eq!(
            expr,
            Expr::BinOp {
                left: Box::new(Expr::BinOp {
                    left: Box::new(Expr::Number(1.0)),
                    op: BinOp::Add,
                    right: Box::new(Expr::Number(2.0)),
                }),
                op: BinOp::Mul,
                right: Box::new(Expr::Number(3.0)),
            }
        );
        Ok(())
    }

    #[test]
    fn if_statement() -> Result<()> {
        let stmt = Parser::new("if 1 then { print 1 } else { print 2 }").parse_stmt()?;
        assert!(matches!(
            stmt,
            Stmt::If {
                condition: Expr::Number(1.0),
                consequent: _,
                alternate: _
            }
        ));
        Ok(())
    }

    #[test]
    fn unclosed_curly() -> Result<()> {
        let stmt = Parser::new("if 1 then { print 1").parse_stmt();
        assert_eq!(stmt, Err(ParseError::InvalidStmt(None)));
        Ok(())
    }

    #[test]
    fn if_else_statement() -> Result<()> {
        let stmt = Parser::new("if x then { print 1 } else { print 0; }").parse_stmt()?;
        assert_eq!(
            stmt,
            Stmt::If {
                condition: Expr::Ident("x".to_string()),
                consequent: Box::new(Stmt::Block(vec![Stmt::Print(Expr::Number(1.0))])),
                alternate: Some(Box::new(Stmt::Block(vec![Stmt::Print(Expr::Number(0.0))]))),
            }
        );
        Ok(())
    }
}
