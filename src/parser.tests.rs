use super::*;

#[test]
fn prog_empty() {
    let ast = Parser::new("").parse_prog();
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn prog_only_semis() {
    let ast = Parser::new(";;;;;").parse_prog();
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn prog_just_vars() {
    let ast = Parser::new("a123; foo_bar; _foo; fooBar;").parse_prog();
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn prog_just_1s() {
    let ast = Parser::new("1 1 1").parse_prog();
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn stmt_let() {
    let ast = Parser::new("let x = 1;").parse_stmt();
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn stmt_print() {
    let ast = Parser::new("print 1").parse_stmt();
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn stmt_let_pat_obj() {
    let ast = Parser::new("let {x, y} = obj;").parse_stmt();
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn stmt_let_pat_obj_nested() {
    let ast = Parser::new("let {x: {y}} = obj;").parse_stmt();
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn stmt_let_pat_obj_rename() {
    let ast = Parser::new("let {x: y} = obj;").parse_stmt();
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn stmt_let_pat_obj_default() {
    let ast = Parser::new("let {x, y = 2} = obj;").parse_stmt();
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn stmt_let_pat_obj_rename_default() {
    let ast = Parser::new("let {x: y = 2} = obj;").parse_stmt();
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn expr_ops() {
    let ast = Parser::new("1 + -2 * 3").parse_expr(0);
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn expr_ops_parens() {
    let ast = Parser::new("(1 + 2) * 3").parse_expr(0);
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn expr_unclosed_paren() {
    let ast = Parser::new("(1 + 2;").parse_expr(0);
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn stmt_if() {
    let ast = Parser::new("if 1 then print 1").parse_stmt();
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn stmt_if_unclosed_curly() {
    let ast = Parser::new("if 1 then { print 1").parse_stmt();
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn stmt_if_else() {
    let ast = Parser::new("if x then { print 1 } else { print 0; }").parse_stmt();
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn stmt_if_else_chain() {
    let ast = Parser::new(
        "
        if x then print 1
        else if y then print 2
        else print 3
        ",
    )
    .parse_stmt();
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn stmt_if_lone_else() {
    let ast = Parser::new("if 1 then a;b; else c;").parse_prog();
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn stmt_print_str() {
    let ast = Parser::new(r#"print "hello\nworld""#).parse_stmt();
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn expr_str_concat() {
    let ast = Parser::new(r#""hello" ++ "world""#).parse_expr(0);
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn expr_obj() {
    let ast = Parser::new("{ a: 1, b: 2 }").parse_expr(0);
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn expr_obj_expr() {
    let ast = Parser::new("{ x: 1 + 2, y: 3 * 4 }").parse_expr(0);
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn expr_prop_access() {
    let ast = Parser::new("obj.a.b.c").parse_expr(0);
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn expr_func_call() {
    let ast = Parser::new("func(a, b)").parse_expr(0);
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn expr_method_call() {
    let ast = Parser::new("obj.method()").parse_expr(0);
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn expr_func_call_missing_paren() {
    let ast = Parser::new("func(a, b").parse_expr(0);
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn expr_func_call_trailing_comma() {
    let ast = Parser::new("func(a, )").parse_expr(0);
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn expr_func_call_missing_comma() {
    let ast = Parser::new("func(a b)").parse_expr(0);
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn expr_func_call_just_comma() {
    let ast = Parser::new("func(,)").parse_expr(0);
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn expr_func_call_missing_arg() {
    let ast = Parser::new("func(,a)").parse_expr(0);
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn expr_func_call_prop_assign_bin_expr() {
    let ast = Parser::new("func().prop = 1 + 2").parse_expr(0);
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn expr_func_call_prop_access() {
    let ast = Parser::new("func().prop").parse_expr(0);
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn expr_obj_empty() {
    let ast = Parser::new("{}").parse_expr(0);
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn expr_obj_key_str() {
    let ast = Parser::new(r#"{ "name": "john", "age": 25 }"#).parse_expr(0);
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn expr_obj_nested() {
    let ast = Parser::new(r#"{ user: { name: "john", age: 25 } }"#).parse_expr(0);
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn expr_op_method_call() {
    let ast = Parser::new("(1 + 2).toString").parse_expr(0);
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn stmt_func_decl() {
    let ast = Parser::new("fn add(a, b) { return }").parse_stmt();
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn stmt_return() {
    let ast = Parser::new("return 42").parse_stmt();
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn stmt_return_no_value() {
    let ast = Parser::new("return").parse_stmt();
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn stmt_func_decl_empty() {
    let ast = Parser::new("fn sayHello() { }").parse_stmt();
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn stmt_func_decl_missing_name() {
    let ast = Parser::new("fn (a, b) { a + b; }").parse_stmt();
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn stmt_func_decl_missing_params() {
    let ast = Parser::new("fn add { 1 + 2; }").parse_stmt();
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn stmt_func_decl_missing_body() {
    let ast = Parser::new("fn add(a, b)").parse_stmt();
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn stmt_func_decl_missing_brace() {
    let ast = Parser::new("fn add(a, b) { a + b").parse_stmt();
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn comments_ignored_in_prog() {
    let src = r"
        // header
        let x = 1; /* mid */ let y = 2;
        print x // tail
        ;
    ";
    let ast = Parser::new(src).parse_prog();
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn comments_inside_expr_and_args() {
    let src = r"
        func(
            1, // after first
            /* block */ 2
        ).prop /* between */ . toString ( /* none */ )
    ";
    let ast = Parser::new(src).parse_expr(0);
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn comments_only_prog() {
    let src = "// just a comment\n/* and a block */";
    let ast = Parser::new(src).parse_prog();
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn expr_comparison_operators() {
    let ast = Parser::new("a < b <= c > d >= e").parse_expr(0);
    insta::assert_debug_snapshot!(ast);
}
