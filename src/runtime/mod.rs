use std::collections::HashMap;
use std::rc::Rc;
use thiserror::Error;

use crate::{
    parser::{
        ParseError, Parser,
        ast::{BinOp, Expr, Prog, Stmt, UnaryOp},
    },
    runtime::state::{Obj, Prim, Type, Val},
};

/// Custom error type for VM operations
#[derive(Error, Debug, PartialEq)]
pub enum RuntimeError {
    #[error("division by zero")]
    DivByZero,

    #[error("unexpected {0:?} in a condition")]
    InvalidCond(Type),

    #[error("invalid {op:?} on {ty:?}")]
    InvalidUnaryOp { op: UnaryOp, ty: Type },

    #[error("invalid {op:?} between {left:?} and {right:?}")]
    InvalidBinaryOp { op: BinOp, left: Type, right: Type },

    #[error("invalid property access on {0:?}")]
    InvalidPropAccess(Type),

    #[error("parse error: {0}")]
    ParseError(#[from] ParseError),
}

/// A single virtual machine instance.
#[derive(Debug)]
pub struct Runtime {
    /// The global object contains all the defined variables as its properties.
    global_obj: Obj,
}

mod state;

impl Runtime {
    pub fn new() -> Self {
        Self {
            global_obj: Obj {
                props: HashMap::new(),
            },
        }
    }

    pub fn exec_str(&mut self, input: &str) -> Result<(), RuntimeError> {
        let prog = Parser::new(input).parse_prog()?;
        self.exec_prog(&prog)
    }

    pub fn exec_prog(&mut self, prog: &Prog) -> Result<(), RuntimeError> {
        for stmt in &prog.stmts {
            self.exec_stmt(stmt)?;
        }
        Ok(())
    }

    fn exec_stmt(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        match stmt {
            Stmt::Block(stmts) => {
                for stmt in stmts {
                    self.exec_stmt(stmt)?;
                }
                Ok(())
            }
            Stmt::Let { ident: name, expr } => {
                let value = self.eval_expr(expr)?;
                self.global_obj.props.insert(name.to_owned(), value);
                Ok(())
            }
            Stmt::Print(expr) => {
                let value = self.eval_expr(expr)?;
                println!("{value:?}");
                Ok(())
            }
            Stmt::If { cond, cons, alt } => {
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
                    Err(RuntimeError::InvalidCond(cond.type_of()))
                }
            }
        }
    }

    fn eval_expr(&self, expr: &Expr) -> Result<Val, RuntimeError> {
        match expr {
            Expr::Num(n) => Ok(Val::Prim(Prim::Num(*n))),
            Expr::Str(s) => Ok(Val::Prim(Prim::Str(s.clone()))),
            Expr::Var(name) => match self.global_obj.props.get(name) {
                Some(value) => Ok(value.clone()),
                None => Ok(Val::Prim(Prim::Nil)),
            },
            Expr::UnaryOp { op, expr } => self.eval_unary_op(*op, expr),
            Expr::BinOp { left, op, right } => self.eval_bin_op(left, *op, right),
            Expr::Obj { props } => {
                let mut obj = Obj {
                    props: HashMap::new(),
                };
                for prop in props {
                    let val = self.eval_expr(&prop.val)?;
                    obj.props.insert(prop.key.clone(), val);
                }
                Ok(Val::Comp(Rc::new(obj)))
            }
            Expr::PropAccess { obj: object, prop } => match self.eval_expr(object)? {
                Val::Comp(obj) => match obj.props.get(prop) {
                    Some(value) => Ok(value.clone()),
                    None => Ok(Val::Prim(Prim::Nil)),
                },
                v @ Val::Prim(_) => Err(RuntimeError::InvalidPropAccess(v.type_of())),
            },
        }
    }

    fn eval_bin_op(&self, left: &Expr, op: BinOp, right: &Expr) -> Result<Val, RuntimeError> {
        let left_val = self.eval_expr(left)?;
        let right_val = self.eval_expr(right)?;

        match (op, left_val, right_val) {
            (BinOp::Add, Val::Prim(Prim::Num(l)), Val::Prim(Prim::Num(r))) => {
                Ok(Val::Prim(Prim::Num(l + r)))
            }

            (BinOp::Sub, Val::Prim(Prim::Num(l)), Val::Prim(Prim::Num(r))) => {
                Ok(Val::Prim(Prim::Num(l - r)))
            }

            (BinOp::Mul, Val::Prim(Prim::Num(l)), Val::Prim(Prim::Num(r))) => {
                Ok(Val::Prim(Prim::Num(l * r)))
            }

            (BinOp::Div, Val::Prim(Prim::Num(l)), Val::Prim(Prim::Num(r))) => {
                if r == 0.0 {
                    return Err(RuntimeError::DivByZero);
                }
                Ok(Val::Prim(Prim::Num(l / r)))
            }

            (BinOp::Eq, l, r) => Ok(Val::Prim(Prim::Bool(l == r))),
            (BinOp::NotEq, l, r) => Ok(Val::Prim(Prim::Bool(l != r))),

            (BinOp::Concat, Val::Prim(Prim::Str(l)), Val::Prim(Prim::Str(r))) => {
                Ok(Val::Prim(Prim::Str(format!("{l}{r}"))))
            }

            (op, l, r) => Err(RuntimeError::InvalidBinaryOp {
                op,
                left: l.type_of(),
                right: r.type_of(),
            }),
        }
    }

    fn eval_unary_op(&self, op: UnaryOp, expr: &Expr) -> Result<Val, RuntimeError> {
        let val = self.eval_expr(expr)?;
        match (op, val) {
            (UnaryOp::Pos, Val::Prim(Prim::Num(n))) => Ok(Val::Prim(Prim::Num(n))),
            (UnaryOp::Neg, Val::Prim(Prim::Num(n))) => Ok(Val::Prim(Prim::Num(-n))),
            (UnaryOp::Not, Val::Prim(Prim::Bool(b))) => Ok(Val::Prim(Prim::Bool(!b))),
            (op, v) => Err(RuntimeError::InvalidUnaryOp {
                op,
                ty: v.type_of(),
            }),
        }
    }
}

#[cfg(test)]
mod tests;
