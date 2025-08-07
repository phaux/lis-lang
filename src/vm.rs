use crate::{ast, parser::ParseError, parser::Parser};
use std::collections::HashMap;
use std::rc::Rc;
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

    #[error("invalid property access on {0:?}")]
    InvalidPropAccess(Type),

    #[error("parse error: {0}")]
    ParseError(#[from] ParseError),
}

/// A single virtual machine instance.
#[derive(Debug)]
pub struct Vm {
    /// The global object contains all the defined variables as its properties.
    global_obj: Obj,
}

/// Any runtime value.
#[derive(Debug, Clone, PartialEq)]
pub enum Val {
    /// A primitive value.
    Prim(Prim),
    /// A composite value.
    Comp(Rc<Obj>),
}

impl Val {
    pub fn type_of(&self) -> Type {
        match self {
            Val::Prim(Prim::Nil) => Type::Nil,
            Val::Prim(Prim::Bool(_)) => Type::Bool,
            Val::Prim(Prim::Num(_)) => Type::Num,
            Val::Prim(Prim::Str(_)) => Type::Str,
            Val::Comp(_) => Type::Obj,
        }
    }
}

/// The type of a value.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Nil,
    Bool,
    Num,
    Str,
    Obj,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Obj {
    props: HashMap<String, Val>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Prim {
    /// A special value that represents the absence of a value.
    Nil,
    Num(f64),
    Bool(bool),
    Str(String),
}

impl Vm {
    pub fn new() -> Self {
        Self {
            global_obj: Obj {
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
            ast::Stmt::If { cond, cons, alt } => {
                let cond = self.eval_expr(cond)?;
                if let Val::Prim(Prim::Bool(cond)) = cond {
                    if cond {
                        self.exec_stmt(cons)
                    } else if let Some(alt) = alt {
                        self.exec_stmt(alt)
                    } else {
                        Ok(())
                    }
                } else {
                    Err(VmError::InvalidCond(cond.type_of()))
                }
            }
        }
    }

    fn eval_expr(&self, expr: &ast::Expr) -> Result<Val, VmError> {
        match expr {
            ast::Expr::Num(n) => Ok(Val::Prim(Prim::Num(*n))),
            ast::Expr::Str(s) => Ok(Val::Prim(Prim::Str(s.clone()))),
            ast::Expr::Var(name) => match self.global_obj.props.get(name) {
                Some(value) => Ok(value.clone()),
                None => Ok(Val::Prim(Prim::Nil)),
            },
            ast::Expr::UnaryOp { op, expr } => self.eval_unary_op(*op, expr),
            ast::Expr::BinOp { left, op, right } => self.eval_bin_op(left, *op, right),
            ast::Expr::Obj { props } => {
                let mut obj = Obj {
                    props: HashMap::new(),
                };
                for prop in props {
                    let val = self.eval_expr(&prop.val)?;
                    obj.props.insert(prop.key.clone(), val);
                }
                Ok(Val::Comp(Rc::new(obj)))
            }
            ast::Expr::PropAccess { obj: object, prop } => match self.eval_expr(object)? {
                Val::Comp(obj) => match obj.props.get(prop) {
                    Some(value) => Ok(value.clone()),
                    None => Ok(Val::Prim(Prim::Nil)),
                },
                v @ Val::Prim(_) => Err(VmError::InvalidPropAccess(v.type_of())),
            },
        }
    }

    fn eval_bin_op(
        &self,
        left: &ast::Expr,
        op: ast::BinOp,
        right: &ast::Expr,
    ) -> Result<Val, VmError> {
        let left_val = self.eval_expr(left)?;
        let right_val = self.eval_expr(right)?;

        match (op, left_val, right_val) {
            (ast::BinOp::Add, Val::Prim(Prim::Num(l)), Val::Prim(Prim::Num(r))) => {
                Ok(Val::Prim(Prim::Num(l + r)))
            }

            (ast::BinOp::Sub, Val::Prim(Prim::Num(l)), Val::Prim(Prim::Num(r))) => {
                Ok(Val::Prim(Prim::Num(l - r)))
            }

            (ast::BinOp::Mul, Val::Prim(Prim::Num(l)), Val::Prim(Prim::Num(r))) => {
                Ok(Val::Prim(Prim::Num(l * r)))
            }

            (ast::BinOp::Div, Val::Prim(Prim::Num(l)), Val::Prim(Prim::Num(r))) => {
                if r == 0.0 {
                    return Err(VmError::DivByZero);
                }
                Ok(Val::Prim(Prim::Num(l / r)))
            }

            (ast::BinOp::Eq, l, r) => Ok(Val::Prim(Prim::Bool(l == r))),
            (ast::BinOp::NotEq, l, r) => Ok(Val::Prim(Prim::Bool(l != r))),

            (ast::BinOp::Concat, Val::Prim(Prim::Str(l)), Val::Prim(Prim::Str(r))) => {
                Ok(Val::Prim(Prim::Str(format!("{l}{r}"))))
            }

            (op, l, r) => Err(VmError::InvalidBinaryOp {
                op,
                left: l.type_of(),
                right: r.type_of(),
            }),
        }
    }

    fn eval_unary_op(&self, op: ast::UnaryOp, expr: &ast::Expr) -> Result<Val, VmError> {
        let val = self.eval_expr(expr)?;
        match (op, val) {
            (ast::UnaryOp::Pos, Val::Prim(Prim::Num(n))) => Ok(Val::Prim(Prim::Num(n))),
            (ast::UnaryOp::Neg, Val::Prim(Prim::Num(n))) => Ok(Val::Prim(Prim::Num(-n))),
            (ast::UnaryOp::Not, Val::Prim(Prim::Bool(b))) => Ok(Val::Prim(Prim::Bool(!b))),
            (op, v) => Err(VmError::InvalidUnaryOp {
                op,
                ty: v.type_of(),
            }),
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
            Some(&Val::Prim(Prim::Num(2.0))),
        );
        assert_eq!(
            vm.global_obj.props.get("bar"),
            Some(&Val::Prim(Prim::Num(3.0))),
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
            Some(&Val::Prim(Prim::Num(1.0))),
        );
        Ok(())
    }

    #[test]
    fn eval_expr() -> Result<(), VmError> {
        let vm = Vm::new();
        let val = vm.eval_expr(&Parser::new(r"1").parse_expr(0)?)?;
        assert_eq!(val, Val::Prim(Prim::Num(1.0)));
        Ok(())
    }

    #[test]
    fn eval_bin_op() -> Result<(), VmError> {
        let vm = Vm::new();
        let val = vm.eval_expr(&Parser::new(r"1 + 2 * 3").parse_expr(0)?)?;
        assert_eq!(val, Val::Prim(Prim::Num(7.0)));
        Ok(())
    }

    #[test]
    fn eval_unary_num_op() -> Result<(), VmError> {
        let vm = Vm::new();
        let val = vm.eval_expr(&Parser::new(r"+-(10)").parse_expr(0)?)?;
        assert_eq!(val, Val::Prim(Prim::Num(-10.0)));
        Ok(())
    }

    #[test]
    fn eval_unary_bool_op() -> Result<(), VmError> {
        let vm = Vm::new();
        let val = vm.eval_expr(&Parser::new(r"!!!(1==1)").parse_expr(0)?)?;
        assert_eq!(val, Val::Prim(Prim::Bool(false)));
        Ok(())
    }

    #[test]
    fn eval_eq_op() -> Result<(), VmError> {
        let vm = Vm::new();
        let val = vm.eval_expr(&Parser::new(r"1 == 1").parse_expr(0)?)?;
        assert_eq!(val, Val::Prim(Prim::Bool(true)));
        let val = vm.eval_expr(&Parser::new(r"1 == 2").parse_expr(0)?)?;
        assert_eq!(val, Val::Prim(Prim::Bool(false)));
        Ok(())
    }

    #[test]
    fn eval_not_eq_op() -> Result<(), VmError> {
        let vm = Vm::new();
        let val = vm.eval_expr(&Parser::new(r"1 != 1").parse_expr(0)?)?;
        assert_eq!(val, Val::Prim(Prim::Bool(false)));
        let val = vm.eval_expr(&Parser::new(r"1 != 2").parse_expr(0)?)?;
        assert_eq!(val, Val::Prim(Prim::Bool(true)));
        Ok(())
    }

    #[test]
    fn assign_string_literal() -> Result<(), VmError> {
        let mut vm = Vm::new();
        vm.exec_str(r#"let foo = "hello world""#)?;
        assert_eq!(
            vm.global_obj.props.get("foo"),
            Some(&Val::Prim(Prim::Str("hello world".to_string()))),
        );
        Ok(())
    }

    #[test]
    fn string_concat() -> Result<(), VmError> {
        let mut vm = Vm::new();
        vm.exec_str(r#"let foo = "hello" ++ "world";"#)?;
        assert_eq!(
            vm.global_obj.props.get("foo"),
            Some(&Val::Prim(Prim::Str("helloworld".to_string()))),
        );
        Ok(())
    }

    #[test]
    fn object_literal() -> Result<(), VmError> {
        let vm = Vm::new();
        let val = vm.eval_expr(&Parser::new(r#"{ name: "john", age: 25 }"#).parse_expr(0)?)?;
        match val {
            Val::Comp(obj) => {
                assert_eq!(
                    obj.props.get("name"),
                    Some(&Val::Prim(Prim::Str("john".to_string())))
                );
                assert_eq!(obj.props.get("age"), Some(&Val::Prim(Prim::Num(25.0))));
            }
            Val::Prim(_) => panic!("Expected object value"),
        }
        Ok(())
    }

    #[test]
    fn object_property_access() -> Result<(), VmError> {
        let mut vm = Vm::new();
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
    fn nested_object() -> Result<(), VmError> {
        let vm = Vm::new();
        let val =
            vm.eval_expr(&Parser::new(r#"{ user: { name: "john", age: 25 } }"#).parse_expr(0)?)?;
        match val {
            Val::Comp(obj) => {
                let user = obj.props.get("user").unwrap();
                match user {
                    Val::Comp(user_obj) => {
                        assert_eq!(
                            user_obj.props.get("name"),
                            Some(&Val::Prim(Prim::Str("john".to_string())))
                        );
                        assert_eq!(user_obj.props.get("age"), Some(&Val::Prim(Prim::Num(25.0))));
                    }
                    Val::Prim(_) => panic!("Expected nested object"),
                }
            }
            Val::Prim(_) => panic!("Expected object value"),
        }
        Ok(())
    }
}
