use crate::ast;
use std::{collections::HashMap, rc::Rc};

/// A single virtual machine instance.
#[derive(Debug)]
pub struct Vm {
    /// The global object contains all the defined variables as its properties.
    global_obj: Object,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Primitive(Primitive),
    Object(Rc<Object>),
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
    Str(String),
}

impl Vm {
    pub fn new() -> Self {
        Self {
            global_obj: Object {
                props: HashMap::new(),
            },
        }
    }

    pub fn exec(&mut self, prog: &ast::Prog) {
        for stmt in &prog.stmts {
            self.exec_stmt(stmt);
        }
    }

    fn exec_stmt(&mut self, stmt: &ast::Stmt) {
        match stmt {
            ast::Stmt::Let { ident: name, expr } => {
                let value = self.eval_expr(expr);
                self.global_obj.props.insert(name.to_owned(), value);
            }
            ast::Stmt::Print(expr) => {
                let value = self.eval_expr(expr);
                println!("{:?}", value);
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
                }
            }
        }
    }
}

#[test]
fn test_vm() {
    let input = r#"
        let foo = 2;
        let bar = 3;
        let baz = 2 * foo + 3 * bar;
    "#;
    let mut parser = crate::parser::Parser::new(input);
    let prog = parser.parse_prog();
    let mut vm = Vm::new();
    vm.exec(&prog);
    assert_eq!(
        vm.global_obj.props.get("baz"),
        Some(&Value::Primitive(Primitive::Num(13.0)))
    );
}
