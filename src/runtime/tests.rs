use super::*;

#[test]
fn assign() -> Result<(), RuntimeError> {
    let mut vm = Runtime::new();
    vm.exec_str(
        r"
                let foo = 2;
                let bar = 3;
            ",
    )?;
    assert_eq!(
        vm.current_scope.vars.borrow().get("foo"),
        Some(&Val::Prim(Prim::Num(2.0))),
    );
    assert_eq!(
        vm.current_scope.vars.borrow().get("bar"),
        Some(&Val::Prim(Prim::Num(3.0))),
    );
    Ok(())
}

#[test]
fn if_stmt() -> Result<(), RuntimeError> {
    let mut vm = Runtime::new();
    vm.exec_str(
        r"
            let foo = 1 + 1;
            if foo == 2 then {
                let bar = 1;
            } else {
                let bar = 0;
            }
            ",
    )?;
    // assert_eq!(
    //     vm.scope.vars.borrow().get("bar"),
    //     Some(&Val::Prim(Prim::Num(1.0))),
    // );
    Ok(())
}

#[test]
fn scopes() -> Result<(), RuntimeError> {
    let mut vm = Runtime::new();
    vm.exec_str(
        r"
        let foo = 1;
        {
            let foo = 2;
            print foo;
        }
        print foo;
        ",
    )?;
    assert_eq!(
        vm.current_scope.vars.borrow().get("foo"),
        Some(&Val::Prim(Prim::Num(1.0))),
    );
    Ok(())
}

#[test]
fn eval_expr() -> Result<(), RuntimeError> {
    let vm = Runtime::new();
    let val = vm.eval_expr(&Parser::new(r"1").parse_expr(0)?)?;
    assert_eq!(val, Val::Prim(Prim::Num(1.0)));
    Ok(())
}

#[test]
fn eval_bin_op() -> Result<(), RuntimeError> {
    let vm = Runtime::new();
    let val = vm.eval_expr(&Parser::new(r"1 + 2 * 3").parse_expr(0)?)?;
    assert_eq!(val, Val::Prim(Prim::Num(7.0)));
    Ok(())
}

#[test]
fn eval_unary_num_op() -> Result<(), RuntimeError> {
    let vm = Runtime::new();
    let val = vm.eval_expr(&Parser::new(r"+-(10)").parse_expr(0)?)?;
    assert_eq!(val, Val::Prim(Prim::Num(-10.0)));
    Ok(())
}

#[test]
fn eval_unary_bool_op() -> Result<(), RuntimeError> {
    let vm = Runtime::new();
    let val = vm.eval_expr(&Parser::new(r"!!!(1==1)").parse_expr(0)?)?;
    assert_eq!(val, Val::Prim(Prim::Bool(false)));
    Ok(())
}

#[test]
fn eval_eq_op() -> Result<(), RuntimeError> {
    let vm = Runtime::new();
    let val = vm.eval_expr(&Parser::new(r"1 == 1").parse_expr(0)?)?;
    assert_eq!(val, Val::Prim(Prim::Bool(true)));
    let val = vm.eval_expr(&Parser::new(r"1 == 2").parse_expr(0)?)?;
    assert_eq!(val, Val::Prim(Prim::Bool(false)));
    Ok(())
}

#[test]
fn eval_not_eq_op() -> Result<(), RuntimeError> {
    let vm = Runtime::new();
    let val = vm.eval_expr(&Parser::new(r"1 != 1").parse_expr(0)?)?;
    assert_eq!(val, Val::Prim(Prim::Bool(false)));
    let val = vm.eval_expr(&Parser::new(r"1 != 2").parse_expr(0)?)?;
    assert_eq!(val, Val::Prim(Prim::Bool(true)));
    Ok(())
}

#[test]
fn assign_string_literal() -> Result<(), RuntimeError> {
    let mut vm = Runtime::new();
    vm.exec_str(r#"let foo = "hello world""#)?;
    assert_eq!(
        vm.current_scope.vars.borrow().get("foo"),
        Some(&Val::Prim(Prim::Str("hello world".to_string()))),
    );
    Ok(())
}

#[test]
fn string_concat() -> Result<(), RuntimeError> {
    let mut vm = Runtime::new();
    vm.exec_str(r#"let foo = "hello" ++ "world";"#)?;
    assert_eq!(
        vm.current_scope.vars.borrow().get("foo"),
        Some(&Val::Prim(Prim::Str("helloworld".to_string()))),
    );
    Ok(())
}

#[test]
fn object_literal() -> Result<(), RuntimeError> {
    let vm = Runtime::new();
    let val = vm.eval_expr(&Parser::new(r#"{ name: "john", age: 25 }"#).parse_expr(0)?)?;
    match val {
        state::Val::Comp(obj) => {
            assert_eq!(
                obj.props.get("name"),
                Some(&Val::Prim(Prim::Str("john".to_string())))
            );
            assert_eq!(obj.props.get("age"), Some(&Val::Prim(Prim::Num(25.0))));
        }
        state::Val::Prim(_) => panic!("Expected object value"),
    }
    Ok(())
}

#[test]
fn object_property_access() -> Result<(), RuntimeError> {
    let mut vm = Runtime::new();
    vm.exec_str(r#"let obj = { name: "john", age: 25 };"#)?;

    // Test property access
    let val = vm.eval_expr(&Parser::new("obj.name").parse_expr(0)?)?;
    assert_eq!(val, Val::Prim(Prim::Str("john".to_string())));

    let val = vm.eval_expr(&Parser::new("obj.age").parse_expr(0)?)?;
    assert_eq!(val, Val::Prim(Prim::Num(25.0)));

    // Test non-existent property
    let val = vm.eval_expr(&Parser::new("obj.nonexistent").parse_expr(0)?)?;
    assert_eq!(val, Val::Prim(Prim::Nil));

    Ok(())
}

#[test]
fn nested_object() -> Result<(), RuntimeError> {
    let vm = Runtime::new();
    let val =
        vm.eval_expr(&Parser::new(r#"{ user: { name: "john", age: 25 } }"#).parse_expr(0)?)?;
    match val {
        state::Val::Comp(obj) => {
            let user = obj.props.get("user").unwrap();
            match user {
                state::Val::Comp(user_obj) => {
                    assert_eq!(
                        user_obj.props.get("name"),
                        Some(&Val::Prim(Prim::Str("john".to_string())))
                    );
                    assert_eq!(user_obj.props.get("age"), Some(&Val::Prim(Prim::Num(25.0))));
                }
                state::Val::Prim(_) => panic!("Expected nested object"),
            }
        }
        state::Val::Prim(_) => panic!("Expected object value"),
    }
    Ok(())
}
