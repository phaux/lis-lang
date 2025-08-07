use super::*;

#[test]
fn empty_prog() -> Result<()> {
    let prog = Parser::new("").parse_prog()?;
    assert_eq!(prog.stmts.len(), 0);
    Ok(())
}

#[test]
fn only_semis_prog() -> Result<()> {
    let prog = Parser::new(";;;;;").parse_prog()?;
    assert_eq!(prog.stmts.len(), 5);
    assert!(prog.stmts.iter().all(|stmt| matches!(stmt, Stmt::Noop)));
    Ok(())
}

#[test]
fn unclosed_paren() {
    let prog = Parser::new("(1 + 2;").parse_expr(0);
    assert_eq!(prog, Err(ParseError::ExprUnclosedParen(Some(Token::Semi))));
}

#[test]
fn lone_else() {
    let prog = Parser::new("if 1 then a;b; else c;").parse_prog();
    assert_eq!(
        prog,
        Err(ParseError::ExprInvalidStart(Some(Token::Keyword(
            Keyword::Else
        ))))
    );
}

#[test]
fn just_1s() {
    let prog = Parser::new("1 1 1").parse_prog();
    // TODO: make this error
    assert_eq!(
        prog,
        Ok(Prog {
            stmts: vec![
                Stmt::Expr(Expr::Num(1.0)),
                Stmt::Expr(Expr::Num(1.0)),
                Stmt::Expr(Expr::Num(1.0)),
            ]
        })
    );
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
    assert_eq!(stmt, Err(ParseError::StmtInvalidStart(None)));
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
fn prop_access() -> Result<()> {
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
fn func_call() -> Result<()> {
    let expr = Parser::new("func(a, b)").parse_expr(0)?;
    assert_eq!(
        expr,
        Expr::FuncCall {
            func: Box::new(Expr::Var("func".to_string())),
            args: vec![Expr::Var("a".to_string()), Expr::Var("b".to_string())]
        }
    );
    Ok(())
}

#[test]
fn method_call() -> Result<()> {
    let expr = Parser::new("obj.method()").parse_expr(0)?;
    assert_eq!(
        expr,
        Expr::FuncCall {
            func: Box::new(Expr::PropAccess {
                obj: Box::new(Expr::Var("obj".to_string())),
                prop: "method".to_string(),
            }),
            args: vec![],
        }
    );
    Ok(())
}

#[test]
fn func_call_missing_paren() {
    let result = Parser::new("func(a, b").parse_expr(0);
    assert_eq!(result, Err(ParseError::ArgInvalidEnd(None)));
}

#[test]
fn func_call_trailing_comma() {
    let result = Parser::new("func(a, )").parse_expr(0);
    assert_eq!(
        result,
        Ok(Expr::FuncCall {
            func: Box::new(Expr::Var("func".to_string())),
            args: vec![Expr::Var("a".to_string())]
        })
    );
}

#[test]
fn func_call_missing_comma() {
    let result = Parser::new("func(a b)").parse_expr(0);
    assert_eq!(
        result,
        Err(ParseError::ArgInvalidEnd(Some(Token::Ident(
            "b".to_string()
        ))))
    );
}

#[test]
fn func_call_just_comma() {
    let result = Parser::new("func(,)").parse_expr(0);
    assert_eq!(
        result,
        Err(ParseError::ExprInvalidStart(Some(Token::Comma)))
    );
}

#[test]
fn func_call_missing_first_arg() {
    let result = Parser::new("func(,a)").parse_expr(0);
    assert_eq!(
        result,
        Err(ParseError::ExprInvalidStart(Some(Token::Comma)))
    );
}

#[test]
fn prop_call_assign_bin_expr() -> Result<()> {
    let expr = Parser::new("func().prop = 1 + 2").parse_expr(0)?;
    assert_eq!(
        expr,
        Expr::Assign {
            place: Box::new(Expr::PropAccess {
                obj: Box::new(Expr::FuncCall {
                    func: Box::new(Expr::Var("func".to_string())),
                    args: vec![],
                }),
                prop: "prop".to_string(),
            }),
            expr: Box::new(Expr::BinOp {
                left: Box::new(Expr::Num(1.0)),
                op: BinOp::Add,
                right: Box::new(Expr::Num(2.0)),
            }),
        }
    );
    Ok(())
}

#[test]
fn call_result_prop_access() -> Result<()> {
    let expr = Parser::new("func().prop").parse_expr(0)?;
    assert_eq!(
        expr,
        Expr::PropAccess {
            obj: Box::new(Expr::FuncCall {
                func: Box::new(Expr::Var("func".to_string())),
                args: vec![],
            }),
            prop: "prop".to_string(),
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
fn function_declaration() -> Result<()> {
    let prog = Parser::new("fn add(a, b) { a + b; }").parse_prog()?;
    assert_eq!(
        prog,
        Prog {
            stmts: vec![Stmt::FuncDecl(ast::FuncDecl {
                name: "add".to_string(),
                params: vec!["a".to_string(), "b".to_string()],
                body: Box::new(Stmt::Block(vec![Stmt::Expr(Expr::BinOp {
                    left: Box::new(Expr::Var("a".to_string())),
                    op: BinOp::Add,
                    right: Box::new(Expr::Var("b".to_string())),
                })])),
            })]
        }
    );
    Ok(())
}

#[test]
fn function_with_no_params() -> Result<()> {
    let prog = Parser::new("fn sayHello() { }").parse_prog()?;
    assert_eq!(
        prog,
        Prog {
            stmts: vec![Stmt::FuncDecl(ast::FuncDecl {
                name: "sayHello".to_string(),
                params: vec![],
                body: Box::new(Stmt::Block(vec![])),
            })]
        }
    );
    Ok(())
}

#[test]
fn function_declaration_errors() {
    // Missing function name
    let prog = Parser::new("fn (a, b) { a + b; }").parse_prog();
    assert!(matches!(prog, Err(ParseError::FnExpectedName(_))));

    // Missing parameters
    let prog = Parser::new("fn add { 1 + 2; }").parse_prog();
    assert!(matches!(prog, Err(ParseError::FnExpectedParen(_))));

    // Missing body
    let prog = Parser::new("fn add(a, b)").parse_prog();
    assert!(matches!(prog, Err(ParseError::FnExpectedBody(_))));

    // Missing closing brace
    let prog = Parser::new("fn add(a, b) { a + b").parse_prog();
    assert!(matches!(prog, Err(ParseError::StmtInvalidStart(_))));
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
