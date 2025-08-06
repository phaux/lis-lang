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
            if let Some(Token::Semi) = self.tokenizer.peek() {
                self.tokenizer.next();
            } else if self.tokenizer.peek().is_some() {
                panic!("expected semicolon at the end of a statement");
            }
        }
        ast::Prog { stmts }
    }

    fn parse_stmt(&mut self) -> ast::Stmt {
        match self.tokenizer.peek() {
            Some(Token::Let) => self.parse_let_stmt(),
            Some(Token::Print) => self.parse_print_stmt(),
            _ => panic!("invalid statement"),
        }
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
        let token = self.tokenizer.peek();

        match token {
            Some(Token::Plus) | Some(Token::Minus) => {
                let op_token = self.tokenizer.next().unwrap();
                let op = ast::UnaryOp::try_from(&op_token).unwrap();
                let expr = self.parse_operand();
                ast::Expr::UnaryOp {
                    op,
                    expr: Box::new(expr),
                }
            }
            _ => {
                let token = self.tokenizer.next();
                match token {
                    Some(Token::Num(n)) => ast::Expr::Number(n),
                    Some(Token::Ident(name)) => ast::Expr::Ident(name),
                    Some(Token::ParenL) => {
                        let expr = self.parse_expr(0);
                        self.expect(Token::ParenR);
                        expr
                    }
                    Some(t) => panic!("invalid expression: unexpected {t:?}"),
                    None => panic!("unexpected end of input"),
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast;

    #[test]
    fn empty_prog() {
        let mut parser = Parser::new("");
        let prog = parser.parse_prog();
        assert_eq!(prog.stmts.len(), 0);
    }

    #[test]
    fn assignment_prog() {
        let mut parser = Parser::new("let x = 1;");
        let prog = parser.parse_prog();
        assert_eq!(
            prog,
            ast::Prog {
                stmts: vec![ast::Stmt::Let {
                    ident: "x".to_string(),
                    expr: ast::Expr::Number(1.0),
                },],
            },
        );
    }

    #[test]
    fn print_prog() {
        let mut parser = Parser::new("print 1");
        let prog = parser.parse_prog();
        assert_eq!(
            prog,
            ast::Prog {
                stmts: vec![ast::Stmt::Print(ast::Expr::Number(1.0)),],
            },
        );
    }

    #[test]
    fn unary_ops() {
        let mut parser = Parser::new("+-1");
        let expr = parser.parse_expr(0);
        assert_eq!(
            expr,
            ast::Expr::UnaryOp {
                op: ast::UnaryOp::Pos,
                expr: Box::new(ast::Expr::UnaryOp {
                    op: ast::UnaryOp::Neg,
                    expr: Box::new(ast::Expr::Number(1.0)),
                }),
            },
        );
    }

    #[test]
    fn binary_ops() {
        let mut parser = Parser::new("1 * 2 + 3 / 4");
        let expr = parser.parse_expr(0);
        assert_eq!(
            expr,
            ast::Expr::BinOp {
                left: Box::new(ast::Expr::BinOp {
                    left: Box::new(ast::Expr::Number(1.0)),
                    op: ast::BinOp::Mul,
                    right: Box::new(ast::Expr::Number(2.0)),
                }),
                op: ast::BinOp::Add,
                right: Box::new(ast::Expr::BinOp {
                    left: Box::new(ast::Expr::Number(3.0)),
                    op: ast::BinOp::Div,
                    right: Box::new(ast::Expr::Number(4.0)),
                }),
            },
        );
    }

    #[test]
    fn binary_and_unary_ops() {
        let mut parser = Parser::new("+1 + -1");
        let expr = parser.parse_expr(0);
        assert_eq!(
            expr,
            ast::Expr::BinOp {
                left: Box::new(ast::Expr::UnaryOp {
                    op: ast::UnaryOp::Pos,
                    expr: Box::new(ast::Expr::Number(1.0)),
                }),
                op: ast::BinOp::Add,
                right: Box::new(ast::Expr::UnaryOp {
                    op: ast::UnaryOp::Neg,
                    expr: Box::new(ast::Expr::Number(1.0)),
                }),
            },
        );
    }

    #[test]
    fn parens() {
        let mut parser = Parser::new("(1 + 2) * 3");
        let expr = parser.parse_expr(0);
        assert_eq!(
            expr,
            ast::Expr::BinOp {
                left: Box::new(ast::Expr::BinOp {
                    left: Box::new(ast::Expr::Number(1.0)),
                    op: ast::BinOp::Add,
                    right: Box::new(ast::Expr::Number(2.0)),
                }),
                op: ast::BinOp::Mul,
                right: Box::new(ast::Expr::Number(3.0)),
            },
        );
    }
}
