//! This module defines the [Parser].
//! It transforms a stream of [Token]s into a tree of [AST](crate::ast) nodes.

use std::{
    fmt,
    iter::{Filter, Peekable},
};

use crate::{
    ast::{BinOp, CompareOp, Expr, Pattern, Prog, Stmt, UnaryOp},
    lexer::Lexer,
    span::Span,
    token::{Keyword, Token},
};

/// Error that can occur during parsing.
#[derive(Debug, PartialEq)]
pub struct ParseError {
    /// Description of what was expected.
    pub expected: &'static str,
    /// Token that caused the error or `None` if the error was caused by end of input.
    pub found: Option<Span<Token>>,
}

impl std::error::Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(tok) = &self.found {
            write!(f, "unexpected {} where {} was expected", tok, self.expected)
        } else {
            write!(
                f,
                "unexpected end of input where {} was expected",
                self.expected
            )
        }
    }
}

pub struct Parser<'a> {
    tokens: Peekable<FilterTokens<'a>>,
}

type FilterTokens<'a> = Filter<Lexer<'a>, fn(&Span<Token>) -> bool>;

impl<'a> Parser<'a> {
    #[must_use]
    pub fn new(input: &'a str) -> Self {
        Parser {
            tokens: Lexer::new(input)
                .filter(Parser::is_not_comment as fn(&Span<Token>) -> bool)
                .peekable(),
        }
    }

    fn is_not_comment(tok: &Span<Token>) -> bool {
        !matches!(tok.node, Token::Comment { .. })
    }

    pub fn parse_prog(&mut self) -> Result<Prog, ParseError> {
        // Parse list of statements until end of input
        let mut stmts = Vec::new();
        let mut needs_separator = false;
        while self.tokens.peek().is_some() {
            // If needs separator - repeat on semicolon or fail
            if needs_separator {
                let tok = self.tokens.next();
                if let Some(tok) = &tok
                    && tok.node == Token::Semi
                {
                    needs_separator = false;
                    continue;
                }
                return Err(ParseError {
                    expected: "semicolon or end of input",
                    found: tok,
                });
            }

            // Parse statement
            let stmt = self.parse_stmt()?;
            needs_separator = stmt.node.needs_separator();
            stmts.push(stmt);
        }

        Ok(Prog { stmts })
    }

    fn parse_stmt(&mut self) -> Result<Span<Stmt>, ParseError> {
        // Parse statement based on first token
        if let Some(tok) = self.tokens.peek() {
            match tok.node {
                Token::CurlyL => return self.parse_block_stmt(),
                Token::Keyword(Keyword::Let) => return self.parse_let_stmt(),
                Token::Keyword(Keyword::Fn) => return self.parse_func_decl(),
                Token::Keyword(Keyword::Print) => return self.parse_print_stmt(),
                Token::Keyword(Keyword::While) => return self.parse_while_stmt(),
                Token::Keyword(Keyword::Return) => return self.parse_return_stmt(),
                Token::Keyword(Keyword::Break) => {
                    let keyword = self.tokens.next().unwrap();
                    return Ok(Span {
                        range: keyword.range.clone(),
                        node: Stmt::Break {
                            keyword: keyword.without_node(),
                        },
                    });
                }
                Token::Keyword(Keyword::Continue) => {
                    let keyword = self.tokens.next().unwrap();
                    return Ok(Span {
                        range: keyword.range.clone(),
                        node: Stmt::Continue {
                            keyword: keyword.without_node(),
                        },
                    });
                }
                _ => {}
            }
        }

        // Parse expression statement
        let expr = Box::new(self.parse_expr(0)?);
        Ok(Span {
            range: expr.range.clone(),
            node: Stmt::Expr { expr },
        })
    }

    fn parse_block_stmt(&mut self) -> Result<Span<Stmt>, ParseError> {
        // Consume opening brace
        let brace_l = self.tokens.next().unwrap().without_node();

        // Parse a list of statements
        let mut stmts = Vec::new();
        let mut needs_separator = false;
        let brace_r = loop {
            // Always break on closing brace
            if let Some(tok) = self.tokens.next_if(|t| t.node == Token::CurlyR) {
                break tok.without_node();
            }

            // If needs separator - repeat on semicolon or fail
            if needs_separator {
                let tok = self.tokens.next();
                if let Some(tok) = &tok
                    && tok.node == Token::Semi
                {
                    needs_separator = false;
                    continue;
                }
                return Err(ParseError {
                    expected: "semicolon or closing brace of block statement",
                    found: tok,
                });
            }

            // Parse statement
            let stmt = self.parse_stmt()?;
            needs_separator = stmt.node.needs_separator();
            stmts.push(stmt);
        };

        Ok(Span {
            range: brace_l.range.start..brace_r.range.end,
            node: Stmt::Block {
                brace_l,
                stmts,
                brace_r,
            },
        })
    }

    fn parse_let_stmt(&mut self) -> Result<Span<Stmt>, ParseError> {
        // Consume 'let' keyword
        let keyword = self.tokens.next().unwrap().without_node();

        // Parse the pattern
        let pat = Box::new(self.parse_pattern()?);

        // Expect '=' after pattern in let statement
        let eq_tok = match self.tokens.next() {
            Some(
                tok @ Span {
                    node: Token::Eq, ..
                },
            ) => tok.without_node(),
            tok => {
                return Err(ParseError {
                    expected: "equals sign of let statement",
                    found: tok,
                });
            }
        };

        // Parse expression
        let init = Box::new(self.parse_expr(0)?);

        Ok(Span {
            range: keyword.range.start..init.range.end,
            node: Stmt::Let {
                keyword,
                pat,
                eq_tok,
                init,
            },
        })
    }

    fn parse_pattern(&mut self) -> Result<Span<Pattern>, ParseError> {
        match self.tokens.next() {
            Some(Span {
                node: Token::Ident { name },
                range,
            }) => Ok(Span {
                range,
                node: Pattern::Ident { name },
            }),
            Some(Span {
                node: Token::CurlyL,
                range,
            }) => self.parse_obj_pat(Span { range, node: () }),
            tok => Err(ParseError {
                expected: "pattern",
                found: tok,
            }),
        }
    }

    fn parse_default_value(&mut self, mut pat: Span<Pattern>) -> Result<Span<Pattern>, ParseError> {
        if let Some(eq_tok) = self.tokens.next_if(|t| t.node == Token::Eq) {
            let default = Box::new(self.parse_expr(0)?);
            pat = Span {
                range: pat.range.start..default.range.end,
                node: Pattern::Default {
                    pat: Box::new(pat),
                    eq_tok: eq_tok.without_node(),
                    default,
                },
            };
        }

        Ok(pat)
    }

    fn parse_obj_pat(&mut self, brace_l: Span<()>) -> Result<Span<Pattern>, ParseError> {
        let mut props = Vec::new();
        let mut needs_separator = false;
        let brace_r = loop {
            // Always break on closing brace
            if let Some(tok) = self.tokens.next_if(|t| t.node == Token::CurlyR) {
                break tok.without_node();
            }

            // If needs separator - repeat on comma or fail
            if needs_separator {
                let tok = self.tokens.next();
                if let Some(tok) = &tok
                    && tok.node == Token::Comma
                {
                    needs_separator = false;
                    continue;
                }
                return Err(ParseError {
                    expected: "comma or closing brace of object pattern",
                    found: tok,
                });
            }

            // Parse property name
            let key = match self.tokens.next() {
                Some(Span {
                    node: Token::Ident { name },
                    range,
                }) => Span { range, node: name },
                tok => {
                    return Err(ParseError {
                        expected: "key identifier of object pattern",
                        found: tok,
                    });
                }
            };

            // Parse optional property value pattern
            let pat = if self.tokens.next_if(|t| t.node == Token::Colon).is_some() {
                self.parse_pattern()?
            } else {
                // No colon - Parse as object property shorthand.
                Span {
                    range: key.range.clone(),
                    node: Pattern::Ident {
                        name: key.node.clone(),
                    },
                }
            };

            let pat = self.parse_default_value(pat)?;

            props.push((key, pat));
            needs_separator = true;
        };

        Ok(Span {
            range: brace_l.range.start..brace_r.range.end,
            node: Pattern::Obj {
                brace_l,
                props,
                brace_r,
            },
        })
    }

    fn parse_func_decl(&mut self) -> Result<Span<Stmt>, ParseError> {
        // Consume 'fn' keyword
        let keyword = self.tokens.next().unwrap().without_node();

        // Parse function name
        let name = match self.tokens.next() {
            Some(Span {
                node: Token::Ident { name },
                range,
            }) => Span { range, node: name },
            tok => {
                return Err(ParseError {
                    expected: "function name identifier",
                    found: tok,
                });
            }
        };

        // Expect opening parenthesis
        let param_paren_l = match self.tokens.next() {
            Some(Span {
                node: Token::ParenL,
                range,
            }) => Span { range, node: () },
            tok => {
                return Err(ParseError {
                    expected: "opening parenthesis of function parameters",
                    found: tok,
                });
            }
        };

        // Parse parameters
        let mut params = Vec::new();
        let mut needs_separator = false;
        let param_paren_r = loop {
            // Always break on closing parenthesis
            if let Some(tok) = self.tokens.next_if(|t| t.node == Token::ParenR) {
                break tok.without_node();
            }

            // If needs separator - repeat on comma or fail
            if needs_separator {
                let tok = self.tokens.next();
                if let Some(tok) = &tok
                    && tok.node == Token::Comma
                {
                    needs_separator = false;
                    continue;
                }
                return Err(ParseError {
                    expected: "comma or closing parenthesis of function parameters",
                    found: tok,
                });
            }

            // Parse parameter pattern
            let pat = self.parse_pattern()?;
            let pat = self.parse_default_value(pat)?;
            params.push(pat);
            needs_separator = true;
        };

        // Parse function body (must be a block statement)
        let body = match self.tokens.peek() {
            Some(Span {
                node: Token::CurlyL,
                ..
            }) => self.parse_block_stmt()?,
            t => {
                return Err(ParseError {
                    expected: "opening brace of function body",
                    found: t.cloned(),
                });
            }
        };

        Ok(Span {
            range: keyword.range.start..body.range.end,
            node: Stmt::FuncDecl {
                keyword,
                name,
                param_paren_l,
                params,
                param_paren_r,
                body: Box::new(body),
            },
        })
    }

    fn parse_print_stmt(&mut self) -> Result<Span<Stmt>, ParseError> {
        // Consume 'print' keyword
        let keyword = self.tokens.next().unwrap();

        // Parse expression
        let expr = Box::new(self.parse_expr(0)?);

        Ok(Span {
            range: keyword.range.start..expr.range.end,
            node: Stmt::Print {
                keyword: keyword.without_node(),
                expr,
            },
        })
    }

    fn parse_return_stmt(&mut self) -> Result<Span<Stmt>, ParseError> {
        // Consume 'return' keyword
        let keyword = self.tokens.next().unwrap();

        // Parse optional expression
        match self.tokens.peek() {
            Some(Span {
                node: Token::Semi | Token::CurlyR,
                ..
            })
            | None => Ok(Span {
                range: keyword.range.clone(),
                node: Stmt::Return {
                    keyword: keyword.without_node(),
                    expr: None,
                },
            }),
            _ => {
                let expr = Box::new(self.parse_expr(0)?);
                Ok(Span {
                    range: keyword.range.start..expr.range.end,
                    node: Stmt::Return {
                        keyword: keyword.without_node(),
                        expr: Some(expr),
                    },
                })
            }
        }
    }

    fn parse_while_stmt(&mut self) -> Result<Span<Stmt>, ParseError> {
        // Consume 'while' keyword
        let keyword = self.tokens.next().unwrap().without_node();

        // Parse condition
        let condition = Box::new(self.parse_expr(0)?);

        // Expect 'do' keyword
        let do_keyword = match self.tokens.next() {
            Some(Span {
                node: Token::Keyword(Keyword::Do),
                range,
            }) => Span { range, node: () },
            tok => {
                return Err(ParseError {
                    expected: "'do' keyword after while condition",
                    found: tok,
                });
            }
        };

        // Parse body
        let body = Box::new(self.parse_stmt()?);

        Ok(Span {
            range: keyword.range.start..body.range.end,
            node: Stmt::While {
                keyword,
                condition,
                do_keyword,
                body,
            },
        })
    }

    pub fn parse_expr(&mut self, precedence: u8) -> Result<Span<Expr>, ParseError> {
        // Parse left operand
        let mut left = self.parse_operand()?;

        // Handle property access and function calls (highest precedence)
        loop {
            left = match self.tokens.peek() {
                Some(Span {
                    node: Token::Dot, ..
                }) => self.parse_prop_access(left)?,
                Some(Span {
                    node: Token::ParenL,
                    ..
                }) => self.parse_func_call(left)?,
                _ => break,
            };
        }

        // Handle assignment expression (right associative)
        if let Some(eq_tok) = self.tokens.next_if(|t| t.node == Token::Eq) {
            let expr = self.parse_expr(0)?;
            left = Span {
                range: left.range.start..expr.range.end,
                node: Expr::Assign {
                    place: Box::new(left),
                    eq_tok: eq_tok.without_node(),
                    expr: Box::new(expr),
                },
            };
        }

        // Handle binary operations
        loop {
            let Some(op_token) = self.tokens.peek() else {
                break;
            };

            if let Some(op) = CompareOp::try_from_token(op_token) {
                let next_prec = CompareOp::get_precedence();
                if next_prec <= precedence {
                    break;
                }

                let op_tok = self.tokens.next().unwrap();
                let mut comparators = vec![(op_tok.with_node(op), self.parse_expr(next_prec)?)];

                while let Some(op_token) = self.tokens.peek() {
                    let Some(op) = CompareOp::try_from_token(op_token) else {
                        break;
                    };
                    // All comparison operators have the same precedence
                    let op_tok = self.tokens.next().unwrap();
                    let right = self.parse_expr(next_prec)?;
                    comparators.push((op_tok.with_node(op), right));
                }

                let end = comparators.last().unwrap().1.range.end;
                left = Span {
                    range: left.range.start..end,
                    node: Expr::CompareChain {
                        left: Box::new(left),
                        comparators,
                    },
                };

                continue;
            }

            let Some(op) = BinOp::try_from_token(op_token) else {
                break;
            };
            let next_prec = op.get_precedence();
            if next_prec <= precedence {
                break;
            }

            let op_tok = self.tokens.next().unwrap(); // Consume operator
            let right = self.parse_expr(next_prec)?;
            left = Span {
                range: left.range.start..right.range.end,
                node: Expr::BinOp {
                    left: Box::new(left),
                    op: op_tok.with_node(op),
                    right: Box::new(right),
                },
            };
        }

        Ok(left)
    }

    fn parse_operand(&mut self) -> Result<Span<Expr>, ParseError> {
        let tok = self.tokens.next();

        if let Some(tok) = &tok {
            // Handle single token expressions
            let expr = match &tok.node {
                Token::Keyword(keyword) => match keyword {
                    Keyword::Nil => Some(Expr::Nil),
                    Keyword::True => Some(Expr::Bool { val: true }),
                    Keyword::False => Some(Expr::Bool { val: false }),
                    _ => None,
                },
                Token::Num { val } => Some(Expr::Num { val: *val }),
                Token::Str { val } => Some(Expr::Str { val: val.clone() }),
                Token::Ident { name } => Some(Expr::Var { name: name.clone() }),
                _ => None,
            };
            if let Some(expr) = expr {
                return Ok(Span {
                    range: tok.range.clone(),
                    node: expr,
                });
            }

            // Handle lambda expression
            if tok.node == Token::Pipe {
                return self.parse_lambda(tok.without_node());
            }

            if let Token::Keyword(Keyword::Do) = tok.node {
                return self.parse_do_expr(tok.without_node());
            }

            if let Token::Keyword(Keyword::If) = tok.node {
                return self.parse_if_expr(tok.without_node());
            }

            // Handle parenthesized expression
            if tok.node == Token::ParenL {
                return self.parse_paren_expr(&tok.without_node());
            }

            // Handle object literal
            if tok.node == Token::CurlyL {
                return self.parse_obj(tok.without_node());
            }

            // Handle unary operator
            if let Some(op) = UnaryOp::try_from_token(tok) {
                let expr = Box::new(self.parse_operand()?);
                return Ok(Span {
                    range: tok.range.start..expr.range.end,
                    node: Expr::UnaryOp {
                        op: tok.with_node(op),
                        expr,
                    },
                });
            }
        }

        Err(ParseError {
            expected: "expression",
            found: tok,
        })
    }

    fn parse_paren_expr(&mut self, paren_l: &Span<()>) -> Result<Span<Expr>, ParseError> {
        let expr = self.parse_expr(0)?;

        let tok = self.tokens.next();
        if let Some(paren_r) = &tok
            && paren_r.node == Token::ParenR
        {
            return Ok(Span {
                range: paren_l.range.start..paren_r.range.end,
                node: expr.node,
            });
        }

        Err(ParseError {
            expected: "closing parenthesis of expression",
            found: tok,
        })
    }

    fn parse_prop_access(&mut self, obj: Span<Expr>) -> Result<Span<Expr>, ParseError> {
        // Consume dot token
        let dot_tok = self.tokens.next().unwrap().without_node();

        // Parse property name
        let prop = match self.tokens.next() {
            Some(Span {
                node: Token::Ident { name },
                range,
            }) => Span { range, node: name },
            tok => {
                return Err(ParseError {
                    expected: "property name identifier of property access",
                    found: tok,
                });
            }
        };

        Ok(Span {
            range: obj.range.start..prop.range.end,
            node: Expr::PropAccess {
                obj: Box::new(obj),
                dot_tok,
                prop,
            },
        })
    }

    fn parse_func_call(&mut self, callee: Span<Expr>) -> Result<Span<Expr>, ParseError> {
        // Consume opening parenthesis
        let paren_l = self.tokens.next().unwrap().without_node();

        // Parse arguments
        let mut args = Vec::new();
        let mut needs_separator = false;
        let paren_r = loop {
            // Always break on closing parenthesis
            if let Some(tok) = self.tokens.next_if(|t| t.node == Token::ParenR) {
                break tok.without_node();
            }

            // If needs separator - repeat on comma or fail
            if needs_separator {
                match self.tokens.next() {
                    Some(Span {
                        node: Token::Comma, ..
                    }) => {
                        needs_separator = false;
                        continue;
                    }
                    tok => {
                        return Err(ParseError {
                            expected: "comma or closing parenthesis of function arguments",
                            found: tok,
                        });
                    }
                }
            }

            // Parse argument
            args.push(self.parse_expr(0)?);
            needs_separator = true;
        };

        Ok(Span {
            range: callee.range.start..paren_r.range.end,
            node: Expr::FuncCall {
                callee: Box::new(callee),
                paren_l,
                args,
                paren_r,
            },
        })
    }

    fn parse_obj(&mut self, brace_l: Span<()>) -> Result<Span<Expr>, ParseError> {
        let mut props = Vec::new();

        let mut needs_separator = false;
        let brace_r = loop {
            // Always break on closing brace
            if let Some(tok) = self.tokens.next_if(|t| t.node == Token::CurlyR) {
                break tok.without_node();
            }

            // If needs separator - repeat on comma or fail
            if needs_separator {
                match self.tokens.next() {
                    Some(Span {
                        node: Token::Comma, ..
                    }) => {
                        needs_separator = false;
                        continue;
                    }
                    tok => {
                        return Err(ParseError {
                            expected: "comma or closing brace of object literal",
                            found: tok,
                        });
                    }
                }
            }

            // Parse property key (identifier or string literal)
            let key = match self.tokens.next() {
                Some(Span {
                    node: Token::Ident { name },
                    range,
                }) => Span { range, node: name },
                Some(Span {
                    node: Token::Str { val },
                    range,
                }) => Span { range, node: val },
                tok => {
                    return Err(ParseError {
                        expected: "property key of object literal",
                        found: tok,
                    });
                }
            };

            // Expect colon
            match self.tokens.next() {
                Some(Span {
                    node: Token::Colon, ..
                }) => {}
                tok => {
                    return Err(ParseError {
                        expected: "colon of object property",
                        found: tok,
                    });
                }
            }

            // Parse property value
            let val = self.parse_expr(0)?;
            props.push((key, val));
            needs_separator = true;
        };

        Ok(Span {
            range: brace_l.range.start..brace_r.range.end,
            node: Expr::Obj {
                brace_l,
                props,
                brace_r,
            },
        })
    }

    fn parse_lambda(&mut self, pipe_l: Span<()>) -> Result<Span<Expr>, ParseError> {
        // Parse parameters
        let mut params = Vec::new();
        let mut needs_separator = false;
        let pipe_r = loop {
            // Always break on closing pipe
            if let Some(tok) = self.tokens.next_if(|t| t.node == Token::Pipe) {
                break tok.without_node();
            }

            // If needs separator - repeat on comma or fail
            if needs_separator {
                let tok = self.tokens.next();
                if let Some(tok) = &tok
                    && tok.node == Token::Comma
                {
                    needs_separator = false;
                    continue;
                }
                return Err(ParseError {
                    expected: "comma or closing pipe of lambda parameters",
                    found: tok,
                });
            }

            // Parse parameter pattern
            let pat = self.parse_pattern()?;
            let pat = self.parse_default_value(pat)?;
            params.push(pat);
            needs_separator = true;
        };

        // Parse lambda body
        let body = self.parse_stmt()?;

        Ok(Span {
            range: pipe_l.range.start..body.range.end,
            node: Expr::Lambda {
                pipe_l,
                params,
                pipe_r,
                body: Box::new(body),
            },
        })
    }

    fn parse_do_expr(&mut self, keyword: Span<()>) -> Result<Span<Expr>, ParseError> {
        // Expect opening brace
        let brace_l = match self.tokens.next() {
            Some(Span {
                node: Token::CurlyL,
                range,
            }) => Span { range, node: () },
            tok => {
                return Err(ParseError {
                    expected: "opening brace of do expression",
                    found: tok,
                });
            }
        };

        // Parse a list of statements
        let mut stmts = Vec::new();
        let mut needs_separator = false;
        let brace_r = loop {
            // Always break on closing brace
            if let Some(tok) = self.tokens.next_if(|t| t.node == Token::CurlyR) {
                break tok.without_node();
            }

            // If needs separator - repeat on semicolon or fail
            if needs_separator {
                let tok = self.tokens.next();
                if let Some(tok) = &tok
                    && tok.node == Token::Semi
                {
                    needs_separator = false;
                    continue;
                }
                return Err(ParseError {
                    expected: "semicolon or closing brace of do expression",
                    found: tok,
                });
            }

            // Parse statement
            let stmt = self.parse_stmt()?;
            needs_separator = stmt.node.needs_separator();
            stmts.push(stmt);
        };

        Ok(Span {
            range: keyword.range.start..brace_r.range.end,
            node: Expr::DoBlock {
                keyword,
                brace_l,
                stmts,
                brace_r,
            },
        })
    }

    fn parse_if_expr(&mut self, keyword: Span<()>) -> Result<Span<Expr>, ParseError> {
        // Parse condition
        let condition = Box::new(self.parse_expr(0)?);

        // Expect 'then'
        let then_keyword = match self.tokens.next() {
            Some(Span {
                node: Token::Keyword(Keyword::Then),
                range,
            }) => Span { range, node: () },
            tok => {
                return Err(ParseError {
                    expected: "then keyword of if expression",
                    found: tok,
                });
            }
        };

        // Parse consequent branch
        let cons_branch = Box::new(self.parse_expr(0)?);

        // Parse optional alternate branch
        let (else_keyword, alt_branch) = match self
            .tokens
            .next_if(|t| t.node == Token::Keyword(Keyword::Else))
        {
            Some(tok) => (
                Some(tok.without_node()),
                Some(Box::new(self.parse_expr(0)?)),
            ),
            None => (None, None),
        };

        let end_node = alt_branch.as_ref().unwrap_or(&cons_branch);

        Ok(Span {
            range: keyword.range.start..end_node.range.end,
            node: Expr::If {
                keyword,
                condition,
                then_keyword,
                cons_branch,
                else_keyword,
                alt_branch,
            },
        })
    }
}

#[cfg(test)]
#[path = "parser.tests.rs"]
mod tests;
