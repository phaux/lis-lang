use super::*;

#[test]
fn prog_simple() {
    let input = "let x = 5;";
    let tokens: Vec<_> = Lexer::new(input).collect();
    insta::assert_debug_snapshot!(tokens);
}

#[test]
fn assign_str_escaped() {
    let input = r#" s = "hello\nworld\twith\\quotes\"" "#;
    let tokens: Vec<_> = Lexer::new(input).collect();
    insta::assert_debug_snapshot!(tokens);
}

#[test]
fn assign_str_unclosed() {
    let input = r#" s = "hello "#;
    let tokens: Vec<_> = Lexer::new(input).collect();
    insta::assert_debug_snapshot!(tokens);
}

#[test]
fn assign_num() {
    let input = " x = 5.5 ";
    let tokens: Vec<_> = Lexer::new(input).collect();
    insta::assert_debug_snapshot!(tokens);
}

#[test]
fn assign_num_invalid() {
    let input = " x = 5.5.5 ";
    let tokens: Vec<_> = Lexer::new(input).collect();
    insta::assert_debug_snapshot!(tokens);
}

#[test]
fn comment_line() {
    let input = "let x = 1; // this is a comment\nprint x";
    let tokens: Vec<_> = Lexer::new(input).collect();
    insta::assert_debug_snapshot!(tokens);
}

#[test]
fn comment_block() {
    let input = "let x = /* block comment */ 1;";
    let tokens: Vec<_> = Lexer::new(input).collect();
    insta::assert_debug_snapshot!(tokens);
}
