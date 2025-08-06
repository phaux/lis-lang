use std::iter::Peekable;

use crate::ast;
use crate::tokenizer::{Token, Tokenizer};

pub struct Parser<'a> {
    tokenizer: Peekable<Tokenizer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Parser {
            tokenizer: Tokenizer::new(input).peekable(),
        }
    }

    fn expect(&mut self, expected: Token) {
        let token = self.tokenizer.peek().unwrap();
        if token == &expected {
            self.tokenizer.next();
        } else {
            panic!("expected {expected:?}, found {token:?}");
        }
    }

    pub fn parse_prog(&mut self) -> ast::Prog {
        let mut stmts = Vec::new();
        while self.tokenizer.peek().is_some() {
            stmts.push(self.parse_stmt());
        }
        ast::Prog { stmts }
    }

    fn parse_stmt(&mut self) -> ast::Stmt {
        let stmt = match self.tokenizer.peek() {
            Some(Token::CurlyL) => self.parse_block_stmt(),
            Some(Token::Let) => self.parse_let_stmt(),
            Some(Token::Print) => self.parse_print_stmt(),
            Some(Token::If) => self.parse_if_stmt(),
            _ => panic!("invalid statement"),
        };
        while self.tokenizer.next_if_eq(&Token::Semi).is_some() {}
        stmt
    }

    fn parse_block_stmt(&mut self) -> ast::Stmt {
        self.tokenizer.next(); // Consume '{'
        let mut stmts = Vec::new();
        loop {
            match self.tokenizer.peek() {
                Some(Token::CurlyR) => break,
                _ => stmts.push(self.parse_stmt()),
            }
        }
        self.expect(Token::CurlyR);
        ast::Stmt::Block(stmts)
    }

    fn parse_let_stmt(&mut self) -> ast::Stmt {
        self.tokenizer.next(); // Consume 'let'
        let token = self.tokenizer.peek().cloned();
        let identifier = match token {
            Some(Token::Ident(name)) => {
                self.tokenizer.next();
                name.clone()
            }
            _ => panic!("expected identifier after 'let'"),
        };
        self.expect(Token::Eq);
        let expr = self.parse_expr(0);
        ast::Stmt::Let {
            ident: identifier.clone(),
            expr,
        }
    }

    fn parse_print_stmt(&mut self) -> ast::Stmt {
        self.tokenizer.next(); // Consume 'print'
        let expr = self.parse_expr(0);
        ast::Stmt::Print(expr)
    }

    fn parse_if_stmt(&mut self) -> ast::Stmt {
        self.tokenizer.next(); // Consume 'if'
        let condition = self.parse_expr(0);

        // Parse then branch
        self.expect(Token::Then);
        let consequent = Box::new(self.parse_stmt());

        // Parse optional else branch
        let alternate = if self.tokenizer.next_if_eq(&Token::Else).is_some() {
            Some(Box::new(self.parse_stmt()))
        } else {
            None
        };

        ast::Stmt::If {
            condition,
            consequent,
            alternate,
        }
    }

    pub fn parse_expr(&mut self, precedence: u8) -> ast::Expr {
        let mut left = self.parse_operand();
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
            let right = self.parse_expr(next_prec);
            left = ast::Expr::BinOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }
        left
    }

    fn parse_operand(&mut self) -> ast::Expr {
        let Some(token) = self.tokenizer.next() else {
            panic!("unexpected end of input");
        };
        if let Ok(op) = ast::UnaryOp::try_from(&token) {
            ast::Expr::UnaryOp {
                op,
                expr: Box::new(self.parse_operand()),
            }
        } else {
            match token {
                Token::Num(n) => ast::Expr::Number(n),
                Token::Ident(name) => ast::Expr::Ident(name),
                Token::ParenL => {
                    let expr = self.parse_expr(0);
                    self.expect(Token::ParenR);
                    expr
                }
                t => panic!("invalid expression: unexpected {t:?}"),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;

    #[test]
    fn empty_prog() {
        let prog = Parser::new("").parse_prog();
        assert_eq!(prog.stmts.len(), 0);
    }

    #[test]
    fn many_semis_prog() {
        let prog = Parser::new("print 1;;;;;").parse_prog();
        assert_eq!(prog.stmts.len(), 1);
    }

    #[test]
    fn assignment_prog() {
        let prog = Parser::new("let x = 1;").parse_prog();
        assert_eq!(
            prog,
            Prog {
                stmts: vec![Stmt::Let {
                    ident: "x".to_string(),
                    expr: Expr::Number(1.0),
                }],
            },
        );
    }

    #[test]
    fn print_stmt() {
        let stmt = Parser::new("print 1").parse_stmt();
        assert_eq!(stmt, Stmt::Print(Expr::Number(1.0)));
    }

    #[test]
    fn unary_ops() {
        let expr = Parser::new("+-1").parse_expr(0);
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
    }

    #[test]
    fn binary_ops() {
        let expr = Parser::new("1 * 2 + 3 / 4").parse_expr(0);
        assert_eq!(
            expr,
            Expr::BinOp {
                left: Box::new(Expr::BinOp {
                    left: Box::new(Expr::Number(1.0)),
                    op: BinOp::Mul,
                    right: Box::new(Expr::Number(2.0)),
                }),
                op: BinOp::Add,
                right: Box::new(Expr::BinOp {
                    left: Box::new(Expr::Number(3.0)),
                    op: BinOp::Div,
                    right: Box::new(Expr::Number(4.0)),
                }),
            },
        );
    }

    #[test]
    fn binary_and_unary_ops() {
        let expr = Parser::new("+1 + -1").parse_expr(0);
        assert_eq!(
            expr,
            Expr::BinOp {
                left: Box::new(Expr::UnaryOp {
                    op: UnaryOp::Pos,
                    expr: Box::new(Expr::Number(1.0)),
                }),
                op: BinOp::Add,
                right: Box::new(Expr::UnaryOp {
                    op: UnaryOp::Neg,
                    expr: Box::new(Expr::Number(1.0)),
                }),
            },
        );
    }

    #[test]
    fn parens() {
        let expr = Parser::new("(1 + 2) * 3").parse_expr(0);
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
            },
        );
    }

    #[test]
    fn if_statement() {
        let stmt = Parser::new("if x then print 1").parse_stmt();
        assert_eq!(
            stmt,
            Stmt::If {
                condition: Expr::Ident("x".to_string()),
                consequent: Box::new(Stmt::Print(Expr::Number(1.0))),
                alternate: None,
            }
        );
    }

    #[test]
    fn if_else_statement() {
        let stmt = Parser::new("if x then { print 1 } else { print 0; }").parse_stmt();
        assert_eq!(
            stmt,
            Stmt::If {
                condition: Expr::Ident("x".to_string()),
                consequent: Box::new(Stmt::Block(vec![Stmt::Print(Expr::Number(1.0))])),
                alternate: Some(Box::new(Stmt::Block(vec![Stmt::Print(Expr::Number(0.0))]))),
            }
        );
    }
}
