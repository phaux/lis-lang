//! This module defines the [Parser].
//! It transforms a stream of [Token]s into a tree of [AST](crate::ast) nodes.

use std::{
    fmt,
    iter::{Filter, Peekable},
};

use crate::{
    ast::{BinOp, Expr, Pat, Prog, Span, Stmt, UnaryOp},
    lexer::Lexer,
    token::{Keyword, Sigil, Token},
};

/// Error that can occur during parsing.
#[derive(Debug, PartialEq)]
pub struct ParseError {
    /// Description of what was expected.
    pub expected: &'static str,
    /// Token that caused the error or `None` if the error was caused by end of input.
    pub found: Option<Token>,
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

type FilterTokens<'a> = Filter<Lexer<'a>, fn(&Token) -> bool>;

impl<'a> Parser<'a> {
    #[must_use]
    pub fn new(input: &'a str) -> Self {
        Parser {
            tokens: Lexer::new(input)
                .filter(Parser::is_not_comment as fn(&Token) -> bool)
                .peekable(),
        }
    }

    fn is_not_comment(tok: &Token) -> bool {
        !matches!(tok.sigil, Sigil::Comment { .. })
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
                    && tok.sigil == Sigil::Semi
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
            match tok.sigil {
                Sigil::CurlyL => return self.parse_block_stmt(),
                Sigil::Keyword(Keyword::Let) => return self.parse_let_stmt(),
                Sigil::Keyword(Keyword::Fn) => return self.parse_func_decl(),
                Sigil::Keyword(Keyword::Print) => return self.parse_print_stmt(),
                Sigil::Keyword(Keyword::If) => return self.parse_if_stmt(),
                Sigil::Keyword(Keyword::While) => return self.parse_while_stmt(),
                Sigil::Keyword(Keyword::Return) => return self.parse_return_stmt(),
                Sigil::Keyword(Keyword::Break) => {
                    let keyword = self.tokens.next().unwrap();
                    return Ok(Span {
                        offset: keyword.offset.clone(),
                        pos: keyword.pos.clone(),
                        node: Stmt::Break { keyword },
                    });
                }
                Sigil::Keyword(Keyword::Continue) => {
                    let keyword = self.tokens.next().unwrap();
                    return Ok(Span {
                        offset: keyword.offset.clone(),
                        pos: keyword.pos.clone(),
                        node: Stmt::Continue { keyword },
                    });
                }
                _ => {}
            }
        }

        // Parse expression statement
        let expr = Box::new(self.parse_expr(0)?);
        Ok(Span {
            offset: expr.offset.clone(),
            pos: expr.pos.clone(),
            node: Stmt::Expr { expr },
        })
    }

    fn parse_block_stmt(&mut self) -> Result<Span<Stmt>, ParseError> {
        // Consume opening brace
        let brace_l = self.tokens.next().unwrap();

        // Parse a list of statements
        let mut stmts = Vec::new();
        let mut needs_separator = false;
        let brace_r = loop {
            // Always break on closing brace
            if let Some(tok) = self.tokens.next_if(|t| t.sigil == Sigil::CurlyR) {
                break tok;
            }

            // If needs separator - repeat on semicolon or fail
            if needs_separator {
                let tok = self.tokens.next();
                if let Some(tok) = &tok
                    && tok.sigil == Sigil::Semi
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
            offset: brace_l.offset.start..brace_r.offset.end,
            pos: brace_l.pos.start..brace_r.pos.end,
            node: Stmt::Block {
                brace_l,
                stmts,
                brace_r,
            },
        })
    }

    fn parse_let_stmt(&mut self) -> Result<Span<Stmt>, ParseError> {
        // Consume 'let' keyword
        let keyword = self.tokens.next().unwrap();

        // Parse the pattern
        let pat = Box::new(self.parse_pattern()?);

        // Expect '=' after pattern in let statement
        let eq_tok = match self.tokens.next() {
            Some(
                tok @ Token {
                    sigil: Sigil::Eq, ..
                },
            ) => tok,
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
            offset: keyword.offset.start..init.offset.end,
            pos: keyword.pos.start..init.pos.end,
            node: Stmt::Let {
                keyword,
                pat,
                eq_tok,
                init,
            },
        })
    }

    fn parse_pattern(&mut self) -> Result<Span<Pat>, ParseError> {
        match self.tokens.next() {
            Some(Token {
                sigil: Sigil::Ident { name },
                offset,
                pos,
            }) => Ok(Span {
                offset,
                pos,
                node: Pat::Ident { name },
            }),
            Some(
                tok @ Token {
                    sigil: Sigil::CurlyL,
                    ..
                },
            ) => self.parse_obj_pat(tok),
            tok => Err(ParseError {
                expected: "pattern",
                found: tok,
            }),
        }
    }

    fn parse_obj_pat(&mut self, brace_l: Token) -> Result<Span<Pat>, ParseError> {
        let mut props = Vec::new();
        let mut needs_separator = false;
        let brace_r = loop {
            // Always break on closing brace
            if let Some(tok) = self.tokens.next_if(|t| t.sigil == Sigil::CurlyR) {
                break tok;
            }

            // If needs separator - repeat on comma or fail
            if needs_separator {
                let tok = self.tokens.next();
                if let Some(tok) = &tok
                    && tok.sigil == Sigil::Comma
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
                Some(Token {
                    sigil: Sigil::Ident { name },
                    offset,
                    pos,
                }) => Span {
                    offset,
                    pos,
                    node: name,
                },
                tok => {
                    return Err(ParseError {
                        expected: "key identifier of object pattern",
                        found: tok,
                    });
                }
            };

            // Parse optional property value pattern
            let mut pat = if self.tokens.next_if(|t| t.sigil == Sigil::Colon).is_some() {
                self.parse_pattern()?
            } else {
                // No colon - Parse as object property shorthand.
                Span {
                    offset: key.offset.clone(),
                    pos: key.pos.clone(),
                    node: Pat::Ident {
                        name: key.node.clone(),
                    },
                }
            };

            // Check for default value after pattern
            if let Some(eq_token) = self.tokens.next_if(|t| t.sigil == Sigil::Eq) {
                let default = Box::new(self.parse_expr(0)?);
                pat = Span {
                    offset: pat.offset.start..default.offset.end,
                    pos: pat.pos.start..default.pos.end,
                    node: Pat::Default {
                        pat: Box::new(pat),
                        eq_tok: eq_token,
                        default,
                    },
                };
            }

            props.push((key.node.clone(), pat));
            needs_separator = true;
        };

        Ok(Span {
            offset: brace_l.offset.start..brace_r.offset.end,
            pos: brace_l.pos.start..brace_r.pos.end,
            node: Pat::Obj {
                brace_l,
                props,
                brace_r,
            },
        })
    }

    fn parse_func_decl(&mut self) -> Result<Span<Stmt>, ParseError> {
        // Consume 'fn' keyword
        let keyword = self.tokens.next().unwrap();

        // Parse function name
        let name = match self.tokens.next() {
            Some(Token {
                sigil: Sigil::Ident { name },
                ..
            }) => name,
            tok => {
                return Err(ParseError {
                    expected: "function name identifier",
                    found: tok,
                });
            }
        };

        // Expect opening parenthesis
        let param_paren_l = match self.tokens.next() {
            Some(
                tok @ Token {
                    sigil: Sigil::ParenL,
                    ..
                },
            ) => tok,
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
            if let Some(tok) = self.tokens.next_if(|t| t.sigil == Sigil::ParenR) {
                break tok;
            }

            // If needs separator - repeat on comma or fail
            if needs_separator {
                let tok = self.tokens.next();
                if let Some(tok) = &tok
                    && tok.sigil == Sigil::Comma
                {
                    needs_separator = false;
                    continue;
                }
                return Err(ParseError {
                    expected: "comma or closing parenthesis of function parameters",
                    found: tok,
                });
            }

            // Parse parameter name
            match self.tokens.next() {
                Some(Token {
                    sigil: Sigil::Ident { name },
                    ..
                }) => params.push(name),
                tok => {
                    return Err(ParseError {
                        expected: "parameter name identifier",
                        found: tok,
                    });
                }
            }
            needs_separator = true;
        };

        // Parse function body (must be a block statement)
        let body = match self.tokens.peek() {
            Some(Token {
                sigil: Sigil::CurlyL,
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
            offset: keyword.offset.start..body.offset.end,
            pos: keyword.pos.start..body.pos.end,
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
            offset: keyword.offset.start..expr.offset.end,
            pos: keyword.pos.start..expr.pos.end,
            node: Stmt::Print { keyword, expr },
        })
    }

    fn parse_return_stmt(&mut self) -> Result<Span<Stmt>, ParseError> {
        // Consume 'return' keyword
        let keyword = self.tokens.next().unwrap();

        // Parse optional expression
        match self.tokens.peek() {
            Some(Token {
                sigil: Sigil::Semi | Sigil::CurlyR,
                ..
            })
            | None => Ok(Span {
                offset: keyword.offset.clone(),
                pos: keyword.pos.clone(),
                node: Stmt::Return {
                    keyword,
                    expr: None,
                },
            }),
            _ => {
                let expr = Box::new(self.parse_expr(0)?);
                Ok(Span {
                    offset: keyword.offset.start..expr.offset.end,
                    pos: keyword.pos.start..expr.pos.end,
                    node: Stmt::Return {
                        keyword,
                        expr: Some(expr),
                    },
                })
            }
        }
    }

    fn parse_if_stmt(&mut self) -> Result<Span<Stmt>, ParseError> {
        // Consume 'if' keyword
        let keyword = self.tokens.next().unwrap();

        // Parse condition
        let condition = Box::new(self.parse_expr(0)?);

        // Expect 'then'
        let then_keyword = match self.tokens.next() {
            Some(
                tok @ Token {
                    sigil: Sigil::Keyword(Keyword::Then),
                    ..
                },
            ) => tok,
            tok => {
                return Err(ParseError {
                    expected: "then keyword of if statement",
                    found: tok,
                });
            }
        };

        // Parse consequent branch
        let cons_branch = Box::new(self.parse_stmt()?);

        // Parse optional alternate branch
        let (else_keyword, alt_branch) = match self
            .tokens
            .next_if(|t| t.sigil == Sigil::Keyword(Keyword::Else))
        {
            Some(tok) => (Some(tok), Some(Box::new(self.parse_stmt()?))),
            None => (None, None),
        };

        let end_node = alt_branch.as_ref().unwrap_or(&cons_branch);

        Ok(Span {
            offset: keyword.offset.start..end_node.offset.end,
            pos: keyword.pos.start..end_node.pos.end,
            node: Stmt::If {
                keyword,
                condition,
                then_keyword,
                cons_branch,
                else_keyword,
                alt_branch,
            },
        })
    }

    fn parse_while_stmt(&mut self) -> Result<Span<Stmt>, ParseError> {
        // Consume 'while' keyword
        let keyword = self.tokens.next().unwrap();

        // Parse condition
        let condition = Box::new(self.parse_expr(0)?);

        // Expect 'do' keyword
        let do_keyword = match self.tokens.next() {
            Some(
                tok @ Token {
                    sigil: Sigil::Keyword(Keyword::Do),
                    ..
                },
            ) => tok,
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
            offset: keyword.offset.start..body.offset.end,
            pos: keyword.pos.start..body.pos.end,
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
                Some(Token {
                    sigil: Sigil::Dot, ..
                }) => self.parse_prop_access(left)?,
                Some(Token {
                    sigil: Sigil::ParenL,
                    ..
                }) => self.parse_func_call(left)?,
                _ => break,
            };
        }

        // Handle assignment expression (right associative)
        if let Some(eq_token) = self.tokens.next_if(|t| t.sigil == Sigil::Eq) {
            let expr = self.parse_expr(0)?;
            left = Span {
                offset: left.offset.start..expr.offset.end,
                pos: left.pos.start..expr.pos.end,
                node: Expr::Assign {
                    place: Box::new(left),
                    eq_tok: eq_token,
                    expr: Box::new(expr),
                },
            };
        }

        // Handle binary operations
        loop {
            let Some(op_token) = self.tokens.peek() else {
                break;
            };
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
                offset: left.offset.start..right.offset.end,
                pos: left.pos.start..right.pos.end,
                node: Expr::BinOp {
                    left: Box::new(left),
                    op,
                    op_tok,
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
            let expr = match &tok.sigil {
                Sigil::Keyword(keyword) => match keyword {
                    Keyword::Nil => Some(Expr::Nil),
                    Keyword::True => Some(Expr::Bool { val: true }),
                    Keyword::False => Some(Expr::Bool { val: false }),
                    _ => None,
                },
                Sigil::Num { val } => Some(Expr::Num { val: *val }),
                Sigil::Str { val } => Some(Expr::Str { val: val.clone() }),
                Sigil::Ident { name } => Some(Expr::Var { name: name.clone() }),
                _ => None,
            };
            if let Some(expr) = expr {
                return Ok(Span {
                    offset: tok.offset.clone(),
                    pos: tok.pos.clone(),
                    node: expr,
                });
            }

            // Handle parenthesized expression
            if tok.sigil == Sigil::ParenL {
                return self.parse_paren_expr(tok);
            }

            // Handle object literal
            if tok.sigil == Sigil::CurlyL {
                return self.parse_obj(tok);
            }

            // Handle unary operator
            if let Some(op) = UnaryOp::try_from_token(tok) {
                let expr = Box::new(self.parse_operand()?);
                return Ok(Span {
                    offset: tok.offset.start..expr.offset.end,
                    pos: tok.pos.start..expr.pos.end,
                    node: Expr::UnaryOp {
                        op,
                        op_tok: tok.clone(),
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

    fn parse_paren_expr(&mut self, paren_l: &Token) -> Result<Span<Expr>, ParseError> {
        let expr = self.parse_expr(0)?;

        let tok = self.tokens.next();
        if let Some(paren_r) = &tok
            && paren_r.sigil == Sigil::ParenR
        {
            return Ok(Span {
                offset: paren_l.offset.start..paren_r.offset.end,
                pos: paren_l.pos.start..paren_r.pos.end,
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
        let dot_tok = self.tokens.next().unwrap();

        // Parse property name
        let prop = match self.tokens.next() {
            Some(Token {
                sigil: Sigil::Ident { name },
                offset,
                pos,
            }) => Span {
                offset,
                pos,
                node: name,
            },
            tok => {
                return Err(ParseError {
                    expected: "property name identifier of property access",
                    found: tok,
                });
            }
        };

        Ok(Span {
            offset: obj.offset.start..prop.offset.end,
            pos: obj.pos.start..prop.pos.end,
            node: Expr::PropAccess {
                obj: Box::new(obj),
                dot_tok,
                prop: prop.node,
            },
        })
    }

    fn parse_func_call(&mut self, callee: Span<Expr>) -> Result<Span<Expr>, ParseError> {
        // Consume opening parenthesis
        let paren_l = self.tokens.next().unwrap();

        // Parse arguments
        let mut args = Vec::new();
        let mut needs_separator = false;
        let paren_r = loop {
            // Always break on closing parenthesis
            if let Some(tok) = self.tokens.next_if(|t| t.sigil == Sigil::ParenR) {
                break tok;
            }

            // If needs separator - repeat on comma or fail
            if needs_separator {
                match self.tokens.next() {
                    Some(Token {
                        sigil: Sigil::Comma,
                        ..
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
            offset: callee.offset.start..paren_r.offset.end,
            pos: callee.pos.start..paren_r.pos.end,
            node: Expr::FuncCall {
                callee: Box::new(callee),
                paren_l,
                args,
                paren_r,
            },
        })
    }

    fn parse_obj(&mut self, brace_l: &Token) -> Result<Span<Expr>, ParseError> {
        let mut props = Vec::new();

        let mut needs_separator = false;
        let brace_r = loop {
            // Always break on closing brace
            if let Some(tok) = self.tokens.next_if(|t| t.sigil == Sigil::CurlyR) {
                break tok;
            }

            // If needs separator - repeat on comma or fail
            if needs_separator {
                match self.tokens.next() {
                    Some(Token {
                        sigil: Sigil::Comma,
                        ..
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
                Some(Token {
                    sigil: Sigil::Ident { name },
                    ..
                }) => name,
                Some(Token {
                    sigil: Sigil::Str { val },
                    ..
                }) => val,
                tok => {
                    return Err(ParseError {
                        expected: "property key of object literal",
                        found: tok,
                    });
                }
            };

            // Expect colon
            match self.tokens.next() {
                Some(Token {
                    sigil: Sigil::Colon,
                    ..
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
            offset: brace_l.offset.start..brace_r.offset.end,
            pos: brace_l.pos.start..brace_r.pos.end,
            node: Expr::Obj {
                brace_l: brace_l.clone(),
                props,
                brace_r: brace_r.clone(),
            },
        })
    }
}

#[cfg(test)]
#[path = "parser.tests.rs"]
mod tests;
