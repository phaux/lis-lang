use crate::parser::Parser;

use super::*;

fn exec_str(global_scope: Rc<Scope>, input: &str) -> Result<(), ExecError> {
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
    exec_str(
        Rc::clone(&scope),
        r"
        foo = foo + 1;
        { foo = foo + 1 };
        foo = foo + 1;
        ",
    )
    .unwrap();
    assert_eq!(scope.lookup("foo"), Some(Val::Num(4.0)),);
}

#[test]
fn assign_chain() {
    let scope = Rc::new(Scope::default());
    scope.declare("foo", Val::default());
    scope.declare("bar", Val::default());
    exec_str(Rc::clone(&scope), r"foo = bar = 1;").unwrap();
    assert_eq!(scope.vars.borrow().get("foo"), Some(&Val::Num(1.0)),);
    assert_eq!(scope.vars.borrow().get("bar"), Some(&Val::Num(1.0)),);
}

#[test]
fn if_stmt() {
    let scope = Rc::new(Scope::default());
    scope.declare("result", Val::default());
    exec_str(
        Rc::clone(&scope),
        r"
        let foo = 1 + 1;
        if foo == 2 then {
            result = 1;
        } else {
            result = 0;
        }
        ",
    )
    .unwrap();
    assert_eq!(scope.lookup("result"), Some(Val::Num(1.0)),);
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
    assert_eq!(scope.lookup("outer"), Some(Val::Num(1.0)),);
    assert_eq!(scope.lookup("inner"), Some(Val::Num(2.0)),);
}

#[test]
fn invalid_assign() {
    let result = exec_str(Rc::new(Scope::default()), r"+1 = +2");
    assert!(matches!(result, Err(ExecError::InvalidAssignment)));
}

#[test]
fn undef_var() {
    let val = eval_str(&Rc::new(Scope::default()), r"foo").unwrap();
    assert_eq!(val, Val::Nil);
}

#[test]
fn eval_numeric() {
    let scope = Rc::new(Scope::default());
    let val = eval_str(&scope, r"- 1 + 2 * + + 3").unwrap();
    assert_eq!(val, Val::Num(5.0));
}

#[test]
fn eval_logical() {
    let scope = Rc::new(Scope::default());
    // TODO: implement and test and/or
    let val = eval_str(&scope, "!!!(1==1)").unwrap();
    assert_eq!(val, Val::Bool(false));
}

#[test]
fn string_concat() {
    let scope = Rc::new(Scope::default());
    let val = eval_str(&scope, r#""hello" ++ "world""#).unwrap();
    assert_eq!(val, Val::Str("helloworld".to_string()));
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
    let val = eval_str(&scope, "obj.nonexistent").unwrap();
    assert_eq!(val, Val::Nil);
}

#[test]
fn nested_object() {
    let scope = Rc::new(Scope::default());
    scope.declare("obj", Val::default());
    exec_str(Rc::clone(&scope), r#"obj = { user: { name: "Alice" } };"#).unwrap();
    // Test nested property access
    let val = eval_str(&scope, "obj.user.name").unwrap();
    assert_eq!(val, Val::Str("Alice".to_string()));
    // Test non-existent nested property
    let val = eval_str(&scope, "obj.user.nonexistent").unwrap();
    assert_eq!(val, Val::Nil);
    // Test non-existent property
    let result = eval_str(&scope, "obj.nonexistent.name");
    assert_eq!(result, Err(ExecError::InvalidPropAccess(Type::Nil)));
}

#[test]
fn func_call() {
    let scope = Rc::new(Scope::default());
    scope.declare("result", Val::default());
    exec_str(
        Rc::clone(&scope),
        r"
        fn add(a, b) {
            return a + b;
        }
        result = add(1, 2);
        ",
    )
    .unwrap();
    assert_eq!(scope.lookup("result"), Some(Val::Num(3.0)));
}

#[test]
fn closure_capture() {
    let scope = Rc::new(Scope::default());
    scope.declare("result", Val::default());
    exec_str(
        Rc::clone(&scope),
        r"
        let x = 10;
        fn closure() {
            return x;
        }
        result = closure();
        ",
    )
    .unwrap();
    assert_eq!(scope.lookup("result"), Some(Val::Num(10.0)));
}

#[test]
fn non_func_call() {
    let scope = Rc::new(Scope::default());
    scope.declare("x", Val::Num(10.0));
    assert_eq!(
        exec_str(Rc::clone(&scope), "x()"),
        Err(ExecError::NotAFunc(Type::Num))
    );
}

#[test]
fn return_outside_func() {
    let scope = Rc::new(Scope::default());
    assert_eq!(
        exec_str(Rc::clone(&scope), "return 10;"),
        Err(ExecError::ReturnOutsideFunc)
    );
}

#[test]
fn higher_order_func() {
    let scope = Rc::new(Scope::default());
    scope.declare("result", Val::default());
    exec_str(
        Rc::clone(&scope),
        r"
        fn executor(f, a, b) {
            return f(a, b);
        }
        fn add(x, y) {
            return x + y;
        }
        result = executor(add, 5, 10);
        ",
    )
    .unwrap();
    assert_eq!(scope.lookup("result"), Some(Val::Num(15.0)));
}

#[test]
fn higher_order_func_return() {
    let scope = Rc::new(Scope::default());
    scope.declare("result", Val::default());
    exec_str(
        Rc::clone(&scope),
        r#"
        fn greeterFactory(greeting) {
            fn greeter(name) {
                return greeting ++ " " ++ name;
            }
            return greeter;
        }
        let helloGreeter = greeterFactory("Hello");
        result = helloGreeter("World");
        "#,
    )
    .unwrap();
    assert_eq!(
        scope.lookup("result"),
        Some(Val::Str("Hello World".to_string())),
    );
}

#[test]
fn recursive_func() {
    let scope = Rc::new(Scope::default());
    scope.declare("result", Val::default());
    exec_str(
        Rc::clone(&scope),
        r"
        fn factorial(n) {
            if n == 0 
            then return 1
            else return n * factorial(n - 1)
        }
        result = factorial(5);
        ",
    )
    .unwrap();
    assert_eq!(scope.lookup("result"), Some(Val::Num(120.0)));
}
