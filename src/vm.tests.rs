use crate::parser::Parser;

use super::*;

fn exec_str(global_scope: &Rc<Scope>, input: &str) -> Result<Val, ExecError> {
    let prog = Parser::new(input).parse_prog().unwrap();
    exec_prog(global_scope, &prog)
}

fn eval_str(scope: &Rc<Scope>, input: &str) -> Result<Val, ExecError> {
    let expr = Parser::new(input).parse_expr(0).unwrap();
    eval_expr_to_val(scope, &expr)
}

#[test]
fn while_loop_with_break() {
    let scope = Rc::new(Scope::root());
    let result = exec_str(
        &scope,
        r"
        let i = 0;
        let sum = 0;
        while i < 10 do {
            if i == 5 then do {
                break;
            } else {
                sum = sum + i;
            }
            i = i + 1;
        }
        return sum;
        ",
    );
    assert_eq!(result.unwrap(), Val::Num(10.0));
}
