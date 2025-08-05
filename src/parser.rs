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
            panic!("expected {:?}, found {:?}", expected, token);
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

    fn parse_expr(&mut self, precedence: u8) -> ast::Expr {
        let mut left = self.parse_bin_operand();
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

    fn parse_bin_operand(&mut self) -> ast::Expr {
        let token = self.tokenizer.next();

        match token {
            Some(Token::Num(n)) => ast::Expr::Number(n),
            Some(Token::Ident(name)) => ast::Expr::Ident(name),
            Some(Token::ParenL) => {
                let expr = self.parse_expr(0);
                self.expect(Token::ParenR);
                expr
            }
            Some(t) => panic!("invalid expression: unexpected {:?}", t),
            None => panic!("unexpected end of input"),
        }
    }
}

#[test]
fn test_parser() {
    use ast::*;
    let input = r#"
        let foo = 1;
        let bar = 2;
        print foo + bar;
    "#;
    let mut parser = Parser::new(input);
    let prog = parser.parse_prog();
    assert_eq!(prog.stmts.len(), 3);
    assert_eq!(
        prog.stmts[0],
        Stmt::Let {
            ident: "foo".to_string(),
            expr: Expr::Number(1.0),
        }
    );
    assert_eq!(
        prog.stmts[1],
        Stmt::Let {
            ident: "bar".to_string(),
            expr: Expr::Number(2.0),
        }
    );
    assert_eq!(
        prog.stmts[2],
        Stmt::Print(Expr::BinOp {
            left: Box::new(Expr::Ident("foo".to_string())),
            op: BinOp::Add,
            right: Box::new(Expr::Ident("bar".to_string())),
        })
    );
}
