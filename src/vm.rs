use crate::{ast, parser::ParseError, parser::Parser};
use std::collections::HashMap;
use thiserror::Error;

/// Custom error type for VM operations
#[derive(Error, Debug, PartialEq)]
pub enum VmError {
    #[error("division by zero")]
    DivByZero,

    #[error("unexpected {0:?} in a condition")]
    InvalidCond(Type),

    #[error("invalid {op:?} on {ty:?}")]
    InvalidUnaryOp { op: ast::UnaryOp, ty: Type },

    #[error("invalid {op:?} between {left:?} and {right:?}")]
    InvalidBinaryOp {
        op: ast::BinOp,
        left: Type,
        right: Type,
    },

    #[error("parse error: {0}")]
    ParseError(#[from] ParseError),
}

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

impl Value {
    pub fn type_of(&self) -> Type {
        match self {
            Value::Primitive(Primitive::Nil) => Type::Nil,
            Value::Primitive(Primitive::Bool(_)) => Type::Bool,
            Value::Primitive(Primitive::Num(_)) => Type::Num,
            // Value::Object(_) => Type::Obj,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Nil,
    Bool,
    Num,
    // Obj,
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

    pub fn exec_str(&mut self, input: &str) -> Result<(), VmError> {
        let prog = Parser::new(input).parse_prog()?;
        self.exec_prog(&prog)
    }

    pub fn exec_prog(&mut self, prog: &ast::Prog) -> Result<(), VmError> {
        for stmt in &prog.stmts {
            self.exec_stmt(stmt)?;
        }
        Ok(())
    }

    fn exec_stmt(&mut self, stmt: &ast::Stmt) -> Result<(), VmError> {
        match stmt {
            ast::Stmt::Block(stmts) => {
                for stmt in stmts {
                    self.exec_stmt(stmt)?;
                }
                Ok(())
            }
            ast::Stmt::Let { ident: name, expr } => {
                let value = self.eval_expr(expr)?;
                self.global_obj.props.insert(name.to_owned(), value);
                Ok(())
            }
            ast::Stmt::Print(expr) => {
                let value = self.eval_expr(expr)?;
                println!("{value:?}");
                Ok(())
            }
            ast::Stmt::If {
                condition,
                consequent: then_branch,
                alternate: else_branch,
            } => {
                let condition = self.eval_expr(condition)?;
                if let Value::Primitive(Primitive::Bool(cond)) = condition {
                    if cond {
                        self.exec_stmt(then_branch)
                    } else if let Some(else_branch) = else_branch {
                        self.exec_stmt(else_branch)
                    } else {
                        Ok(())
                    }
                } else {
                    Err(VmError::InvalidCond(condition.type_of()))
                }
            }
        }
    }

    fn eval_expr(&self, expr: &ast::Expr) -> Result<Value, VmError> {
        match expr {
            ast::Expr::Number(n) => Ok(Value::Primitive(Primitive::Num(*n))),
            ast::Expr::Ident(name) => match self.global_obj.props.get(name) {
                Some(value) => Ok(value.clone()),
                None => Ok(Value::Primitive(Primitive::Nil)),
            },
            ast::Expr::UnaryOp { op, expr } => {
                let val = self.eval_expr(expr)?;
                match op {
                    ast::UnaryOp::Pos => match val {
                        Value::Primitive(Primitive::Num(n)) => {
                            Ok(Value::Primitive(Primitive::Num(n)))
                        }
                        Value::Primitive(_) => Err(VmError::InvalidUnaryOp {
                            op: *op,
                            ty: val.type_of(),
                        }),
                    },
                    ast::UnaryOp::Neg => match val {
                        Value::Primitive(Primitive::Num(n)) => {
                            Ok(Value::Primitive(Primitive::Num(-n)))
                        }
                        Value::Primitive(_) => Err(VmError::InvalidUnaryOp {
                            op: *op,
                            ty: val.type_of(),
                        }),
                    },
                    ast::UnaryOp::Not => match val {
                        Value::Primitive(Primitive::Bool(b)) => {
                            Ok(Value::Primitive(Primitive::Bool(!b)))
                        }
                        Value::Primitive(_) => Err(VmError::InvalidUnaryOp {
                            op: *op,
                            ty: val.type_of(),
                        }),
                    },
                }
            }
            ast::Expr::BinOp { left, op, right } => {
                let left_val = self.eval_expr(left)?;
                let right_val = self.eval_expr(right)?;
                match op {
                    ast::BinOp::Add => match (&left_val, &right_val) {
                        (
                            Value::Primitive(Primitive::Num(l)),
                            Value::Primitive(Primitive::Num(r)),
                        ) => Ok(Value::Primitive(Primitive::Num(l + r))),
                        _ => Err(VmError::InvalidBinaryOp {
                            op: *op,
                            left: left_val.type_of(),
                            right: right_val.type_of(),
                        }),
                    },
                    ast::BinOp::Sub => match (&left_val, &right_val) {
                        (
                            Value::Primitive(Primitive::Num(l)),
                            Value::Primitive(Primitive::Num(r)),
                        ) => Ok(Value::Primitive(Primitive::Num(l - r))),
                        _ => Err(VmError::InvalidBinaryOp {
                            op: *op,
                            left: left_val.type_of(),
                            right: right_val.type_of(),
                        }),
                    },
                    ast::BinOp::Mul => match (&left_val, &right_val) {
                        (
                            Value::Primitive(Primitive::Num(l)),
                            Value::Primitive(Primitive::Num(r)),
                        ) => Ok(Value::Primitive(Primitive::Num(l * r))),
                        _ => Err(VmError::InvalidBinaryOp {
                            op: *op,
                            left: left_val.type_of(),
                            right: right_val.type_of(),
                        }),
                    },
                    ast::BinOp::Div => match (&left_val, &right_val) {
                        (
                            Value::Primitive(Primitive::Num(l)),
                            Value::Primitive(Primitive::Num(r)),
                        ) => {
                            if *r == 0.0 {
                                return Err(VmError::DivByZero);
                            }
                            Ok(Value::Primitive(Primitive::Num(l / r)))
                        }
                        _ => Err(VmError::InvalidBinaryOp {
                            op: *op,
                            left: left_val.type_of(),
                            right: right_val.type_of(),
                        }),
                    },
                    ast::BinOp::Eq => Ok(Value::Primitive(Primitive::Bool(left_val == right_val))),
                    ast::BinOp::NotEq => {
                        Ok(Value::Primitive(Primitive::Bool(left_val != right_val)))
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn assign() -> Result<(), VmError> {
        let mut vm = Vm::new();
        vm.exec_str(
            r"
                let foo = 2;
                let bar = 3;
            ",
        )?;
        assert_eq!(
            vm.global_obj.props.get("foo"),
            Some(&Value::Primitive(Primitive::Num(2.0))),
        );
        assert_eq!(
            vm.global_obj.props.get("bar"),
            Some(&Value::Primitive(Primitive::Num(3.0))),
        );
        Ok(())
    }

    #[test]
    fn if_stmt() -> Result<(), VmError> {
        let mut vm = Vm::new();
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
        assert_eq!(
            vm.global_obj.props.get("bar"),
            Some(&Value::Primitive(Primitive::Num(1.0))),
        );
        Ok(())
    }

    #[test]
    fn eval_expr() -> Result<(), VmError> {
        let vm = Vm::new();
        let val = vm.eval_expr(&Parser::new(r"1").parse_expr(0)?)?;
        assert_eq!(val, Value::Primitive(Primitive::Num(1.0)));
        Ok(())
    }

    #[test]
    fn eval_bin_op() -> Result<(), VmError> {
        let vm = Vm::new();
        let val = vm.eval_expr(&Parser::new(r"1 + 2 * 3").parse_expr(0)?)?;
        assert_eq!(val, Value::Primitive(Primitive::Num(7.0)));
        Ok(())
    }

    #[test]
    fn eval_unary_num_op() -> Result<(), VmError> {
        let vm = Vm::new();
        let val = vm.eval_expr(&Parser::new(r"+-(10)").parse_expr(0)?)?;
        assert_eq!(val, Value::Primitive(Primitive::Num(-10.0)));
        Ok(())
    }

    #[test]
    fn eval_unary_bool_op() -> Result<(), VmError> {
        let vm = Vm::new();
        let val = vm.eval_expr(&Parser::new(r"!!!(1==1)").parse_expr(0)?)?;
        assert_eq!(val, Value::Primitive(Primitive::Bool(false)));
        Ok(())
    }

    #[test]
    fn eval_eq_op() -> Result<(), VmError> {
        let vm = Vm::new();
        let val = vm.eval_expr(&Parser::new(r"1 == 1").parse_expr(0)?)?;
        assert_eq!(val, Value::Primitive(Primitive::Bool(true)));
        let val = vm.eval_expr(&Parser::new(r"1 == 2").parse_expr(0)?)?;
        assert_eq!(val, Value::Primitive(Primitive::Bool(false)));
        Ok(())
    }

    #[test]
    fn eval_not_eq_op() -> Result<(), VmError> {
        let vm = Vm::new();
        let val = vm.eval_expr(&Parser::new(r"1 != 1").parse_expr(0)?)?;
        assert_eq!(val, Value::Primitive(Primitive::Bool(false)));
        let val = vm.eval_expr(&Parser::new(r"1 != 2").parse_expr(0)?)?;
        assert_eq!(val, Value::Primitive(Primitive::Bool(true)));
        Ok(())
    }
}
