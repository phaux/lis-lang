use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use thiserror::Error;

mod state;

use crate::{
    parser::{
        ParseError, Parser,
        ast::{BinOp, Expr, Prog, Stmt, UnaryOp},
    },
    runtime::state::{Obj, Prim, Scope, Type, Val},
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

    #[error("undefined variable {name:?}")]
    UndefVar { name: String },

    #[error("invalid assignment lhs")]
    InvalidAssignment,

    #[error("parse error: {0}")]
    ParseError(#[from] ParseError),
}

/// A single virtual machine instance.
#[derive(Debug)]
pub struct Runtime {
    current_scope: Rc<Scope>,
}

impl Runtime {
    pub fn new() -> Self {
        Self {
            current_scope: Rc::new(Scope {
                vars: RefCell::new(HashMap::new()),
                parent: None,
            }),
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
            Stmt::Noop => Ok(()),
            Stmt::Expr(expr) => {
                self.eval_expr(expr)?;
                Ok(())
            }
            Stmt::Block(stmts) => {
                let prev_scope = Rc::clone(&self.current_scope);
                self.current_scope = Rc::new(Scope {
                    vars: RefCell::new(HashMap::new()),
                    parent: Some(prev_scope),
                });
                for stmt in stmts {
                    self.exec_stmt(stmt)?;
                }
                self.current_scope = self
                    .current_scope
                    .parent
                    .clone()
                    .expect("block scope must have a parent");
                Ok(())
            }
            Stmt::Let { ident: name, expr } => {
                let value = self.eval_expr(expr)?;
                self.current_scope
                    .vars
                    .borrow_mut()
                    .insert(name.to_owned(), value);
                Ok(())
            }
            Stmt::FuncDecl(_func) => {
                todo!()
            }
            Stmt::Return(_expr) => todo!(),
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
            Expr::Var(name) => {
                let mut current = Some(&self.current_scope);
                while let Some(scope) = current {
                    if let Some(value) = scope.vars.borrow().get(name) {
                        return Ok(value.clone());
                    }
                    current = scope.parent.as_ref();
                }
                Ok(Val::Prim(Prim::Nil))
            }
            Expr::Assign { place, expr } => self.exec_assign(place, expr),
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
            Expr::FuncCall { func: _, args: _ } => {
                todo!()
            }
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

    fn exec_assign(&self, place: &Expr, expr: &Expr) -> Result<Val, RuntimeError> {
        let Expr::Var(name) = place else {
            return Err(RuntimeError::InvalidAssignment);
        };
        let val = self.eval_expr(expr)?;

        // Find the variable in the current or parent scopes and update it
        let mut current = Some(&self.current_scope);
        while let Some(scope) = current {
            if scope.vars.borrow().contains_key(name) {
                scope
                    .vars
                    .borrow_mut()
                    .insert(name.to_string(), val.clone());
                return Ok(val);
            }
            current = scope.parent.as_ref();
        }

        // If variable not found, return error
        Err(RuntimeError::UndefVar {
            name: name.to_string(),
        })
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
