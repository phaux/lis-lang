use crate::{ast, parser::Parser};
use std::collections::HashMap;

/// A single virtual machine instance.
#[derive(Debug)]
pub struct Vm {
    /// The global object contains all the defined variables as its properties.
    global_obj: Object,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Primitive(Primitive),
    // Object(Rc<Object>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Object {
    props: HashMap<String, Value>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Primitive {
    /// A special value that represents the absence of a value.
    Nil,
    Num(f64),
    Bool(bool),
}

impl Vm {
    pub fn new() -> Self {
        Self {
            global_obj: Object {
                props: HashMap::new(),
            },
        }
    }

    pub fn exec_str(&mut self, input: &str) {
        let prog = Parser::new(input).parse_prog();
        self.exec_prog(&prog);
    }

    pub fn exec_prog(&mut self, prog: &ast::Prog) {
        for stmt in &prog.stmts {
            self.exec_stmt(stmt);
        }
    }

    fn exec_stmt(&mut self, stmt: &ast::Stmt) {
        match stmt {
            ast::Stmt::Block(stmts) => {
                for stmt in stmts {
                    self.exec_stmt(stmt);
                }
            }
            ast::Stmt::Let { ident: name, expr } => {
                let value = self.eval_expr(expr);
                self.global_obj.props.insert(name.to_owned(), value);
            }
            ast::Stmt::Print(expr) => {
                let value = self.eval_expr(expr);
                println!("{value:?}");
            }
            ast::Stmt::If {
                condition,
                consequent: then_branch,
                alternate: else_branch,
            } => {
                let condition = self.eval_expr(condition);
                if let Value::Primitive(Primitive::Bool(cond)) = condition {
                    if cond {
                        self.exec_stmt(then_branch);
                    } else if let Some(else_branch) = else_branch {
                        self.exec_stmt(else_branch);
                    }
                } else {
                    panic!("if condition must evaluate to a boolean");
                }
            }
        }
    }

    fn eval_expr(&self, expr: &ast::Expr) -> Value {
        match expr {
            ast::Expr::Number(n) => Value::Primitive(Primitive::Num(*n)),
            ast::Expr::Ident(name) => match self.global_obj.props.get(name) {
                Some(value) => value.clone(),
                None => Value::Primitive(Primitive::Nil),
            },
            ast::Expr::UnaryOp { op, expr } => {
                let val = self.eval_expr(expr);
                match op {
                    ast::UnaryOp::Pos => match val {
                        Value::Primitive(Primitive::Num(n)) => Value::Primitive(Primitive::Num(n)),
                        _ => panic!("type mismatch"),
                    },
                    ast::UnaryOp::Neg => match val {
                        Value::Primitive(Primitive::Num(n)) => Value::Primitive(Primitive::Num(-n)),
                        _ => panic!("type mismatch"),
                    },
                    ast::UnaryOp::Not => match val {
                        Value::Primitive(Primitive::Bool(b)) => {
                            Value::Primitive(Primitive::Bool(!b))
                        }
                        _ => panic!("type mismatch"),
                    },
                }
            }
            ast::Expr::BinOp { left, op, right } => {
                let left_val = self.eval_expr(left);
                let right_val = self.eval_expr(right);
                match op {
                    ast::BinOp::Add => match (left_val, right_val) {
                        (
                            Value::Primitive(Primitive::Num(l)),
                            Value::Primitive(Primitive::Num(r)),
                        ) => Value::Primitive(Primitive::Num(l + r)),
                        _ => panic!("type mismatch"),
                    },
                    ast::BinOp::Sub => match (left_val, right_val) {
                        (
                            Value::Primitive(Primitive::Num(l)),
                            Value::Primitive(Primitive::Num(r)),
                        ) => Value::Primitive(Primitive::Num(l - r)),
                        _ => panic!("type mismatch"),
                    },
                    ast::BinOp::Mul => match (left_val, right_val) {
                        (
                            Value::Primitive(Primitive::Num(l)),
                            Value::Primitive(Primitive::Num(r)),
                        ) => Value::Primitive(Primitive::Num(l * r)),
                        _ => panic!("type mismatch"),
                    },
                    ast::BinOp::Div => match (left_val, right_val) {
                        (
                            Value::Primitive(Primitive::Num(l)),
                            Value::Primitive(Primitive::Num(r)),
                        ) => Value::Primitive(Primitive::Num(l / r)),
                        _ => panic!("type mismatch"),
                    },
                    ast::BinOp::Eq => Value::Primitive(Primitive::Bool(left_val == right_val)),
                    ast::BinOp::NotEq => Value::Primitive(Primitive::Bool(left_val != right_val)),
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::Parser;

    #[test]
    fn assign() {
        let mut vm = Vm::new();
        vm.exec_prog(
            &Parser::new(
                r#"
                    let foo = 2;
                    let bar = 3;
                "#,
            )
            .parse_prog(),
        );
        assert_eq!(
            vm.global_obj.props.get("foo"),
            Some(&Value::Primitive(Primitive::Num(2.0))),
        );
        assert_eq!(
            vm.global_obj.props.get("bar"),
            Some(&Value::Primitive(Primitive::Num(3.0))),
        );
    }

    #[test]
    fn if_stmt() {
        let mut vm = Vm::new();
        vm.exec_str(
            r#"
            let foo = 1 + 1;
            if foo == 2 then {
                let bar = 1;
            } else {
                let bar = 0;
            }
            "#,
        );
        assert_eq!(
            vm.global_obj.props.get("bar"),
            Some(&Value::Primitive(Primitive::Num(1.0))),
        );
    }

    #[test]
    fn eval_expr() {
        let vm = Vm::new();
        let val = vm.eval_expr(&Parser::new(r#"1;"#).parse_expr(0));
        assert_eq!(val, Value::Primitive(Primitive::Num(1.0)));
    }

    #[test]
    fn eval_bin_op() {
        let vm = Vm::new();
        let val = vm.eval_expr(&Parser::new(r#"1 + 2 * 3"#).parse_expr(0));
        assert_eq!(val, Value::Primitive(Primitive::Num(7.0)));
    }

    #[test]
    fn eval_unary_num_op() {
        let vm = Vm::new();
        let val = vm.eval_expr(&Parser::new(r#"+-(10)"#).parse_expr(0));
        assert_eq!(val, Value::Primitive(Primitive::Num(-10.0)));
    }

    #[test]
    fn eval_unary_bool_op() {
        let vm = Vm::new();
        let val = vm.eval_expr(&Parser::new(r#"!!!(1==1)"#).parse_expr(0));
        assert_eq!(val, Value::Primitive(Primitive::Bool(false)));
    }

    #[test]
    fn eval_eq_op() {
        let vm = Vm::new();
        let val = vm.eval_expr(&Parser::new(r#"1 == 1"#).parse_expr(0));
        assert_eq!(val, Value::Primitive(Primitive::Bool(true)));
        let val = vm.eval_expr(&Parser::new(r#"1 == 2"#).parse_expr(0));
        assert_eq!(val, Value::Primitive(Primitive::Bool(false)));
    }

    #[test]
    fn eval_not_eq_op() {
        let vm = Vm::new();
        let val = vm.eval_expr(&Parser::new(r#"1 != 1"#).parse_expr(0));
        assert_eq!(val, Value::Primitive(Primitive::Bool(false)));
        let val = vm.eval_expr(&Parser::new(r#"1 != 2"#).parse_expr(0));
        assert_eq!(val, Value::Primitive(Primitive::Bool(true)));
    }
}
