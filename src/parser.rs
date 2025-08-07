use std::iter::Peekable;
use thiserror::Error;

use crate::ast;
use crate::tokenizer::{Token, Tokenizer};

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
            tok => return Err(ParseError::InvalidStmtStart(tok.cloned())),
        }?;
        // consume semis
        while self.tokenizer.next_if_eq(&Token::Semi).is_some() {}
        Ok(stmt)
    }

    fn parse_block_stmt(&mut self) -> Result<ast::Stmt> {
        self.tokenizer.next(); // Consume '{'
        let mut stmts = Vec::new();
        while self.tokenizer.next_if_eq(&Token::CurlyR).is_none() {
            stmts.push(self.parse_stmt()?);
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
        }
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
            let Some(tok) = self.tokenizer.peek() else {
                break;
            };
            let Some(op) = ast::BinOp::try_from_token(tok) else {
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
            Some(Token::Num(n)) => Ok(ast::Expr::Num(n)),
            Some(Token::Ident(name)) => Ok(ast::Expr::Var(name)),
            Some(Token::Str(s)) => Ok(ast::Expr::Str(s)),
            Some(Token::ParenL) => {
                let expr = self.parse_expr(0)?;
                let paren = self.tokenizer.next();
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

        while self.tokenizer.next_if_eq(&Token::CurlyR).is_none() {
            // Parse property key (identifier or string literal)
            let key = match self.tokenizer.next() {
                Some(Token::Ident(name)) => name,
                Some(Token::Str(s)) => s,
                t => return Err(ParseError::InvalidObjKey(t)),
            };

            // Expect colon
            match self.tokenizer.next() {
                Some(Token::Colon) => {}
                t => return Err(ParseError::InvalidObjColon(t)),
            }

            // Parse property value
            let val = self.parse_expr(0)?;
            props.push(ast::Prop { key, val });

            // Check for comma or closing brace
            match self.tokenizer.peek() {
                Some(Token::Comma) => {
                    self.tokenizer.next(); // consume comma
                }
                Some(Token::CurlyR) => {
                    self.tokenizer.next(); // consume closing brace
                    break;
                }
                t => return Err(ParseError::InvalidObjEnd(t.cloned())),
            }
        }

        Ok(ast::Expr::Obj { props })
    }

    fn parse_prop_access(&mut self, mut obj: ast::Expr) -> Result<ast::Expr> {
        while self.tokenizer.next_if_eq(&Token::Dot).is_some() {
            let prop = match self.tokenizer.next() {
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
    fn only_semis_prog() {
        let prog = Parser::new(";;;;").parse_prog();
        assert_eq!(prog, Err(ParseError::InvalidStmtStart(Some(Token::Semi))));
    }

    #[test]
    fn unclosed_paren() {
        let prog = Parser::new("(1 + 2;").parse_expr(0);
        assert_eq!(prog, Err(ParseError::UnclosedExprParen(Some(Token::Semi))));
    }

    #[test]
    fn assignment_prog() -> Result<()> {
        let prog = Parser::new("let x = 1;").parse_prog()?;
        assert_eq!(
            prog,
            Prog {
                stmts: vec![Stmt::Let {
                    ident: "x".to_string(),
                    expr: Expr::Num(1.0),
                }],
            },
        );
        Ok(())
    }

    #[test]
    fn print_stmt() -> Result<()> {
        let stmt = Parser::new("print 1").parse_stmt()?;
        assert_eq!(stmt, Stmt::Print(Expr::Num(1.0)));
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
                    expr: Box::new(Expr::Num(1.0))
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
                left: Box::new(Expr::Num(1.0)),
                op: BinOp::Add,
                right: Box::new(Expr::BinOp {
                    left: Box::new(Expr::Num(2.0)),
                    op: BinOp::Mul,
                    right: Box::new(Expr::Num(3.0)),
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
                left: Box::new(Expr::Num(1.0)),
                op: BinOp::Add,
                right: Box::new(Expr::BinOp {
                    left: Box::new(Expr::UnaryOp {
                        op: UnaryOp::Neg,
                        expr: Box::new(Expr::Num(2.0)),
                    }),
                    op: BinOp::Mul,
                    right: Box::new(Expr::Num(3.0)),
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
                    left: Box::new(Expr::Num(1.0)),
                    op: BinOp::Add,
                    right: Box::new(Expr::Num(2.0)),
                }),
                op: BinOp::Mul,
                right: Box::new(Expr::Num(3.0)),
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
                cond: Expr::Num(1.0),
                cons: _,
                alt: _
            }
        ));
        Ok(())
    }

    #[test]
    fn unclosed_curly() {
        let stmt = Parser::new("if 1 then { print 1").parse_stmt();
        assert_eq!(stmt, Err(ParseError::InvalidStmtStart(None)));
    }

    #[test]
    fn if_else_statement() -> Result<()> {
        let stmt = Parser::new("if x then { print 1 } else { print 0; }").parse_stmt()?;
        assert_eq!(
            stmt,
            Stmt::If {
                cond: Expr::Var("x".to_string()),
                cons: Box::new(Stmt::Block(vec![Stmt::Print(Expr::Num(1.0))])),
                alt: Some(Box::new(Stmt::Block(vec![Stmt::Print(Expr::Num(0.0))]))),
            }
        );
        Ok(())
    }

    #[test]
    fn print_string_literal() -> Result<()> {
        let stmt = Parser::new(r#"print "hello\nworld""#).parse_stmt()?;
        assert_eq!(stmt, Stmt::Print(Expr::Str("hello\nworld".to_string())));
        Ok(())
    }

    #[test]
    fn string_concat() -> Result<()> {
        let expr = Parser::new(r#""hello" ++ "world""#).parse_expr(0)?;
        assert_eq!(
            expr,
            Expr::BinOp {
                left: Box::new(Expr::Str("hello".to_string())),
                op: BinOp::Concat,
                right: Box::new(Expr::Str("world".to_string())),
            }
        );
        Ok(())
    }

    #[test]
    fn object_literal() -> Result<()> {
        let expr = Parser::new("{ a: 1, b: 2 }").parse_expr(0)?;
        assert_eq!(
            expr,
            Expr::Obj {
                props: vec![
                    Prop {
                        key: "a".to_string(),
                        val: Expr::Num(1.0),
                    },
                    Prop {
                        key: "b".to_string(),
                        val: Expr::Num(2.0),
                    },
                ],
            }
        );
        Ok(())
    }

    #[test]
    fn object_property_access() -> Result<()> {
        let expr = Parser::new("obj.a.b.c").parse_expr(0)?;
        assert_eq!(
            expr,
            Expr::PropAccess {
                obj: Box::new(Expr::PropAccess {
                    obj: Box::new(Expr::PropAccess {
                        obj: Box::new(Expr::Var("obj".to_string())),
                        prop: "a".to_string(),
                    }),
                    prop: "b".to_string(),
                }),
                prop: "c".to_string(),
            }
        );
        Ok(())
    }

    #[test]
    fn empty_object() -> Result<()> {
        let expr = Parser::new("{}").parse_expr(0)?;
        assert_eq!(expr, Expr::Obj { props: vec![] });
        Ok(())
    }

    #[test]
    fn object_with_string_keys() -> Result<()> {
        let expr = Parser::new(r#"{ "name": "john", "age": 25 }"#).parse_expr(0)?;
        assert_eq!(
            expr,
            Expr::Obj {
                props: vec![
                    Prop {
                        key: "name".to_string(),
                        val: Expr::Str("john".to_string()),
                    },
                    Prop {
                        key: "age".to_string(),
                        val: Expr::Num(25.0),
                    },
                ],
            }
        );
        Ok(())
    }

    #[test]
    fn nested_object() -> Result<()> {
        let expr = Parser::new("{ user: { name: \"john\", age: 25 } }").parse_expr(0)?;
        assert_eq!(
            expr,
            Expr::Obj {
                props: vec![Prop {
                    key: "user".to_string(),
                    val: Expr::Obj {
                        props: vec![
                            Prop {
                                key: "name".to_string(),
                                val: Expr::Str("john".to_string()),
                            },
                            Prop {
                                key: "age".to_string(),
                                val: Expr::Num(25.0),
                            },
                        ],
                    },
                }],
            }
        );
        Ok(())
    }

    #[test]
    fn property_access_with_expressions() -> Result<()> {
        let expr = Parser::new("(1 + 2).toString").parse_expr(0)?;
        assert_eq!(
            expr,
            Expr::PropAccess {
                obj: Box::new(Expr::BinOp {
                    left: Box::new(Expr::Num(1.0)),
                    op: BinOp::Add,
                    right: Box::new(Expr::Num(2.0)),
                }),
                prop: "toString".to_string(),
            }
        );
        Ok(())
    }

    #[test]
    fn object_with_expressions() -> Result<()> {
        let expr = Parser::new("{ x: 1 + 2, y: 3 * 4 }").parse_expr(0)?;
        assert_eq!(
            expr,
            Expr::Obj {
                props: vec![
                    Prop {
                        key: "x".to_string(),
                        val: Expr::BinOp {
                            left: Box::new(Expr::Num(1.0)),
                            op: BinOp::Add,
                            right: Box::new(Expr::Num(2.0)),
                        },
                    },
                    Prop {
                        key: "y".to_string(),
                        val: Expr::BinOp {
                            left: Box::new(Expr::Num(3.0)),
                            op: BinOp::Mul,
                            right: Box::new(Expr::Num(4.0)),
                        },
                    },
                ],
            }
        );
        Ok(())
    }
}
