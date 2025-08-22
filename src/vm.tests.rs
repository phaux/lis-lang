use crate::parser::Parser;

use super::*;

fn exec_str(global_scope: Rc<Scope>, input: &str) -> Result<Val, ExecError> {
    let prog = Parser::new(input).parse_prog().unwrap();
    exec_prog(global_scope, &prog)
}

fn eval_str(scope: &Rc<Scope>, input: &str) -> Result<Val, ExecError> {
    let expr = Parser::new(input).parse_expr(0).unwrap();
    eval_expr(scope, &expr)
}

#[test]
fn assign_self() {
    let scope = Rc::new(Scope::default());
    scope.declare("foo", Val::Num(1.0));
    let result = exec_str(
        Rc::clone(&scope),
        r"
        foo = foo + 1;
        { foo = foo + 1; }
        foo = foo + 1;
        return foo;
        ",
    )
    .unwrap();
    assert_eq!(result, Val::Num(4.0));
}

#[test]
fn assign_chain() {
    let scope = Rc::new(Scope::default());
    scope.declare("foo", Val::default());
    scope.declare("bar", Val::default());
    exec_str(Rc::clone(&scope), r"foo = bar = 1;").unwrap();
    assert_eq!(scope.lookup("foo"), Some(Val::Num(1.0)));
    assert_eq!(scope.lookup("bar"), Some(Val::Num(1.0)));
}

#[test]
fn if_stmt() {
    let scope = Rc::new(Scope::default());
    let result = exec_str(
        Rc::clone(&scope),
        r"
        let foo = 1 + 1;
        if foo == 2 then {
            return 1;
        } else {
            return 0;
        }
        ",
    )
    .unwrap();
    assert_eq!(result, Val::Num(1.0));
}

#[test]
fn if_non_bool_cond() {
    let result = exec_str(Rc::new(Scope::default()), r"if 1 then return");
    assert!(matches!(
        result,
        Err(ExecError {
            pos: Pos { line: 0, col: 3 },
            scope: _,
            kind: ExecErrorKind::InvalidCondition { cond_ty: Type::Num },
        })
    ));
}

#[test]
fn block_scopes() {
    let scope = Rc::new(Scope::default());
    scope.declare("outer", Val::default());
    scope.declare("inner", Val::default());
    exec_str(
        Rc::clone(&scope),
        r"
        let foo = 1;
        {
            let foo = 2;
            inner = foo;
        }
        outer = foo;
        ",
    )
    .unwrap();
    assert_eq!(scope.lookup("outer"), Some(Val::Num(1.0)));
    assert_eq!(scope.lookup("inner"), Some(Val::Num(2.0)));
}

#[test]
fn assign_non_literal() {
    let result = exec_str(Rc::new(Scope::default()), r"+1 = +2");
    assert!(matches!(
        result,
        Err(ExecError {
            pos: Pos { line: 0, col: 0 },
            scope: _,
            kind: ExecErrorKind::InvalidAssign,
        })
    ));
}

#[test]
fn assign_undef() {
    let result = exec_str(Rc::new(Scope::default()), r"foo = 1");
    assert!(matches!(
        result,
        Err(ExecError {
            pos: Pos { line: 0, col: 0 },
            scope: _,
            kind: ExecErrorKind::UndefVar { name },
        }) if name == "foo"
    ));
}

#[test]
fn obj_pat() {
    let scope = Rc::new(Scope::default());
    let result = exec_str(
        Rc::clone(&scope),
        r"
        let o = {x: 1, y: 2};
        let {x, y} = o;
        return x + y;
        ",
    )
    .unwrap();
    assert_eq!(result, Val::Num(3.0));
}

#[test]
fn obj_pat_nested() {
    let scope = Rc::new(Scope::default());
    let result = exec_str(
        Rc::clone(&scope),
        r"
        let o = {a: {b: {c: 100}}};
        let {a: {b: {c}}} = o;
        return c;
        ",
    )
    .unwrap();
    assert_eq!(result, Val::Num(100.0));
}

#[test]
fn obj_pat_rename() {
    let scope = Rc::new(Scope::default());
    let result = exec_str(
        Rc::clone(&scope),
        r"
        let o = {x: 1};
        let {x: y} = o;
        return y;
        ",
    )
    .unwrap();
    assert_eq!(result, Val::Num(1.0));
}

#[test]
fn obj_pat_default() {
    let scope = Rc::new(Scope::default());
    let result = exec_str(
        Rc::clone(&scope),
        r"
        let o = {x: 1};
        let {x = 0, y = 2} = o;
        return x + y;
        ",
    )
    .unwrap();
    assert_eq!(result, Val::Num(3.0));
}

#[test]
fn obj_pat_nested_default() {
    let scope = Rc::new(Scope::default());
    let result = exec_str(
        Rc::clone(&scope),
        r"
        let o = {x: {a: 1}};
        let {x: {a = 0, b = 2} = {}} = o;
        return a + b;
        ",
    )
    .unwrap();

    assert_eq!(result, Val::Num(3.0));
}

#[test]
fn obj_pat_default_with_nil() {
    let scope = Rc::new(Scope::default());
    let result = exec_str(
        Rc::clone(&scope),
        r"
        let o = {a: nil, b: 2};
        let {a = 1, b = 0} = o;
        return a + b;
        ",
    )
    .unwrap();
    assert_eq!(result, Val::Num(3.0));
}

#[test]
fn obj_pat_non_obj() {
    let result = exec_str(Rc::new(Scope::default()), r"let {x, y} = 1;");
    assert!(matches!(
        result,
        Err(ExecError {
            pos: Pos { line: 0, col: 4 },
            scope: _,
            kind: ExecErrorKind::InvalidMatchObj {
                matched_ty: Type::Num
            },
        })
    ));
}

#[test]
fn var_undef() {
    let val = eval_str(&Rc::new(Scope::default()), r"foo").unwrap();
    assert_eq!(val, Val::Nil);
}

#[test]
fn op_numeric() {
    let scope = Rc::new(Scope::default());
    let val = eval_str(&scope, r"- 1 + 2 * + + 3").unwrap();
    assert_eq!(val, Val::Num(5.0));
}

#[test]
fn op_unary_non_num() {
    let scope = Rc::new(Scope::default());
    assert!(matches!(
        eval_str(&scope, "-true"),
        Err(ExecError {
            pos: Pos { line: 0, col: 0 },
            scope: _,
            kind: ExecErrorKind::InvalidUnaryOp {
                op: UnaryOp::Neg,
                val_ty: Type::Bool,
            },
        })
    ));
}

#[test]
fn op_logical_and() {
    let scope = Rc::new(Scope::default());
    assert_eq!(eval_str(&scope, "true and true").unwrap(), Val::Bool(true));
    assert_eq!(
        eval_str(&scope, "true and false").unwrap(),
        Val::Bool(false)
    );
}

#[test]
fn op_logical_or() {
    let scope = Rc::new(Scope::default());
    assert_eq!(eval_str(&scope, "false or true").unwrap(), Val::Bool(true));
    assert_eq!(
        eval_str(&scope, "false or false").unwrap(),
        Val::Bool(false)
    );
}

#[test]
fn op_logical_precedence() {
    let scope = Rc::new(Scope::default());
    assert_eq!(
        eval_str(&scope, "false and true or true").unwrap(),
        Val::Bool(true)
    );
    assert_eq!(
        eval_str(&scope, "true or true and false").unwrap(),
        Val::Bool(true)
    );
}

#[test]
fn op_logical_comparison() {
    let scope = Rc::new(Scope::default());
    assert_eq!(
        eval_str(&scope, "1 == 1 and 2 == 2").unwrap(),
        Val::Bool(true)
    );
    assert_eq!(
        eval_str(&scope, "1 != 1 or 2 == 2").unwrap(),
        Val::Bool(true)
    );
}

#[test]
fn op_logical_short_circuiting() {
    let scope = Rc::new(Scope::new(Rc::new(Scope::default())));
    scope.declare("side_effect", Val::Num(0.0));

    // Should not evaluate the second part due to short-circuiting
    eval_str(&scope, "false and (side_effect = side_effect + 1.0) == 1.0").unwrap();
    assert_eq!(scope.lookup("side_effect").unwrap(), Val::Num(0.0));

    // Should evaluate the second part
    eval_str(&scope, "true and (side_effect = side_effect + 1.0) == 1.0").unwrap();
    assert_eq!(scope.lookup("side_effect").unwrap(), Val::Num(1.0));
}

#[test]
fn op_logical_non_bool() {
    let scope = Rc::new(Scope::default());
    assert!(matches!(
        eval_str(&scope, "1 and true"),
        Err(ExecError {
            pos: Pos { line: 0, col: 2 },
            scope: _,
            kind: ExecErrorKind::InvalidBinOp {
                op: BinOp::And,
                l_ty: Type::Num,
                r_ty: Type::Bool,
            },
        })
    ));
    assert!(matches!(
        eval_str(&scope, r#""string" or false"#),
        Err(ExecError {
            pos: Pos { line: 0, col: 9 },
            scope: _,
            kind: ExecErrorKind::InvalidBinOp {
                op: BinOp::Or,
                l_ty: Type::Str,
                r_ty: Type::Bool,
            },
        })
    ));
}

#[test]
fn op_logical_not() {
    let scope = Rc::new(Scope::default());
    assert_eq!(eval_str(&scope, "!!!(1==1)").unwrap(), Val::Bool(false));
}

#[test]
fn op_concat_string() {
    let scope = Rc::new(Scope::default());
    let val = eval_str(&scope, r#""hello" ++ "world""#).unwrap();
    assert_eq!(val, Val::Str("helloworld".to_string()));
}

#[test]
fn op_concat_non_string() {
    let scope = Rc::new(Scope::default());
    assert!(matches!(
        eval_str(&scope, "1 ++ 2"),
        Err(ExecError {
            pos: Pos { line: 0, col: 2 },
            scope: _,
            kind: ExecErrorKind::InvalidBinOp {
                op: BinOp::Concat,
                l_ty: Type::Num,
                r_ty: Type::Num,
            },
        })
    ));
}

#[test]
fn prop_access() {
    let scope = Rc::new(Scope::default());
    scope.declare("obj", Val::default());
    exec_str(Rc::clone(&scope), r#"obj = { name: "john", age: 25 };"#).unwrap();
    // Test property access
    let val = eval_str(&scope, "obj.name").unwrap();
    assert_eq!(val, Val::Str("john".to_string()));
    let val = eval_str(&scope, "obj.age").unwrap();
    assert_eq!(val, Val::Num(25.0));
    // Test non-existent property
    let val = eval_str(&scope, "obj.non_existent").unwrap();
    assert_eq!(val, Val::Nil);
}

#[test]
fn prop_access_nested() {
    let scope = Rc::new(Scope::default());
    scope.declare("obj", Val::default());
    exec_str(Rc::clone(&scope), r#"obj = { user: { name: "Alice" } };"#).unwrap();
    // Test nested property access
    let val = eval_str(&scope, "obj.user.name").unwrap();
    assert_eq!(val, Val::Str("Alice".to_string()));
    // Test non-existent nested property
    let val = eval_str(&scope, "obj.user.non_existent").unwrap();
    assert_eq!(val, Val::Nil);
    // Test non-existent property
    let result = eval_str(&scope, "obj.non_existent.name");
    assert!(matches!(
        result,
        Err(ExecError {
            pos: Pos { line: 0, col: 16 },
            scope: _,
            kind: ExecErrorKind::InvalidPropAccess { obj_ty: Type::Nil },
        })
    ));
}

#[test]
fn func_call() {
    let scope = Rc::new(Scope::default());
    let result = exec_str(
        Rc::clone(&scope),
        r"
        fn add(a, b) {
            return a + b;
        }
        return add(1, 2);
        ",
    )
    .unwrap();
    assert_eq!(result, Val::Num(3.0));
}

#[test]
fn func_closure_capture() {
    let scope = Rc::new(Scope::default());
    let result = exec_str(
        Rc::clone(&scope),
        r"
        let x = 10;
        fn closure() {
            return x;
        }
        return closure();
        ",
    )
    .unwrap();
    assert_eq!(result, Val::Num(10.0));
}

#[test]
fn func_call_non_func() {
    let scope = Rc::new(Scope::default());
    scope.declare("x", Val::Num(10.0));
    assert!(matches!(
        exec_str(Rc::clone(&scope), "x()"),
        Err(ExecError {
            pos: Pos { line: 0, col: 0 },
            scope: _,
            kind: ExecErrorKind::InvalidCall {
                called_ty: Type::Num,
            },
        })
    ));
}

#[test]
fn func_higher_order() {
    let scope = Rc::new(Scope::default());
    let result = exec_str(
        Rc::clone(&scope),
        r"
        fn executor(f, a, b) {
            return f(a, b);
        }
        fn add(x, y) {
            return x + y;
        }
        return executor(add, 5, 10);
        ",
    )
    .unwrap();
    assert_eq!(result, Val::Num(15.0));
}

#[test]
fn func_higher_order_return() {
    let scope = Rc::new(Scope::default());
    let result = exec_str(
        Rc::clone(&scope),
        r#"
        fn greeterFactory(greeting) {
            fn greeter(name) {
                return greeting ++ " " ++ name;
            }
            return greeter;
        }
        let helloGreeter = greeterFactory("Hello");
        return helloGreeter("World");
        "#,
    )
    .unwrap();
    assert_eq!(result, Val::Str("Hello World".to_string()));
}

#[test]
fn func_recursive() {
    let scope = Rc::new(Scope::default());
    let result = exec_str(
        Rc::clone(&scope),
        r"
        fn factorial(n) {
            if n == 0 
            then return 1
            else return n * factorial(n - 1)
        }
        return factorial(5);
        ",
    )
    .unwrap();
    assert_eq!(result, Val::Num(120.0));
}

#[test]
fn func_too_few_args() {
    let scope = Rc::new(Scope::default());
    scope.declare("a1", Val::default());
    scope.declare("a2", Val::default());
    exec_str(
        Rc::clone(&scope),
        r"
        fn foo(x, y) {
            a1 = x;
            a2 = y;
        }
        foo(1)
        ",
    )
    .unwrap();
    assert_eq!(scope.lookup("a1"), Some(Val::Num(1.0)));
    assert_eq!(scope.lookup("a2"), Some(Val::Nil));
}

#[test]
fn func_too_many_args() {
    let scope = Rc::new(Scope::default());
    let result = exec_str(
        Rc::clone(&scope),
        r"
        fn foo(x, y) {
            return x + y;
        }
        return foo(1, 2, 3);
        ",
    )
    .unwrap();
    assert_eq!(result, Val::Num(3.0));
}
