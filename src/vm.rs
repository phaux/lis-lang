use std::{collections::HashMap, rc::Rc};

use thiserror::Error;

use crate::{
    ast::{BinOp, Expr, Prog, Stmt, UnaryOp},
    state::{Func, Obj, Scope, Type, Val},
};

/// Error which can happen during program execution.
#[derive(Error, Debug, PartialEq)]
pub enum ExecError {
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

    #[error("return statement outside of a function")]
    ReturnOutsideFunc,

    #[error("not a function: {0:?}")]
    NotAFunc(Type),
}

#[must_use]
#[derive(Debug, PartialEq)]
enum ExecResult {
    Void,
    Return(Val),
}

pub fn exec_prog(global_scope: Rc<Scope>, prog: &Prog) -> Result<(), ExecError> {
    let prog_scope = Rc::new(Scope::new(global_scope));
    for stmt in &prog.stmts {
        match exec_stmt(Rc::clone(&prog_scope), stmt)? {
            ExecResult::Void => {}
            ExecResult::Return(_) => return Err(ExecError::ReturnOutsideFunc),
        }
    }
    Ok(())
}

fn exec_stmt(scope: Rc<Scope>, stmt: &Stmt) -> Result<ExecResult, ExecError> {
    match stmt {
        Stmt::Noop => Ok(ExecResult::Void),
        Stmt::Expr(expr) => {
            eval_expr(&scope, expr)?;
            Ok(ExecResult::Void)
        }
        Stmt::Block(stmts) => {
            let block_scope = Rc::new(Scope::new(scope));
            for stmt in stmts {
                if let ExecResult::Return(val) = exec_stmt(Rc::clone(&block_scope), stmt)? {
                    return Ok(ExecResult::Return(val));
                }
            }
            Ok(ExecResult::Void)
        }
        Stmt::Let { ident: name, expr } => {
            let val = eval_expr(&scope, expr)?;
            scope.declare(name, val);
            Ok(ExecResult::Void)
        }
        Stmt::FuncDecl(func) => {
            scope.declare(
                &func.name,
                Val::Func(Rc::new(Func::new(
                    Rc::clone(&scope),
                    func.params.clone(),
                    Rc::new(*func.body.clone()),
                ))),
            );
            Ok(ExecResult::Void)
        }
        Stmt::Return(expr) => {
            let val = if let Some(expr) = expr {
                eval_expr(&scope, expr)?
            } else {
                Val::Nil
            };
            Ok(ExecResult::Return(val))
        }
        Stmt::Print(expr) => {
            let value = eval_expr(&scope, expr)?;
            println!("{value:?}");
            Ok(ExecResult::Void)
        }
        Stmt::If { cond, cons, alt } => {
            let cond = eval_expr(&scope, cond)?;
            if let Val::Bool(cond) = cond {
                if cond {
                    exec_stmt(scope, cons)
                } else if let Some(alt) = alt {
                    exec_stmt(scope, alt)
                } else {
                    Ok(ExecResult::Void)
                }
            } else {
                Err(ExecError::InvalidCond(cond.type_of()))
            }
        }
    }
}

fn eval_expr(scope: &Rc<Scope>, expr: &Expr) -> Result<Val, ExecError> {
    match expr {
        Expr::Num(n) => Ok(Val::Num(*n)),
        Expr::Str(s) => Ok(Val::Str(s.clone())),
        Expr::Var(name) => Ok(scope.lookup(name).unwrap_or_default()),
        Expr::Assign { place, expr } => exec_assign(scope, place, expr),
        Expr::UnaryOp { op, expr } => eval_unary_op(scope, *op, expr),
        Expr::BinOp { left, op, right } => eval_bin_op(scope, left, *op, right),
        Expr::Obj { props } => {
            let mut obj = Obj {
                props: HashMap::new(),
            };
            for prop in props {
                let val = eval_expr(scope, &prop.val)?;
                obj.props.insert(prop.key.clone(), val);
            }
            Ok(Val::Obj(Rc::new(obj)))
        }
        Expr::PropAccess { obj: object, prop } => match eval_expr(scope, object)? {
            Val::Obj(obj) => match obj.props.get(prop) {
                Some(value) => Ok(value.clone()),
                None => Ok(Val::Nil),
            },
            v => Err(ExecError::InvalidPropAccess(v.type_of())),
        },
        Expr::FuncCall { func, args } => {
            let func = eval_expr(scope, func)?;
            let Val::Func(func) = func else {
                return Err(ExecError::NotAFunc(func.type_of()));
            };

            let mut arg_values = Vec::new();
            for arg in args {
                arg_values.push(eval_expr(scope, arg)?);
            }

            let func_scope = Scope::new(Rc::clone(&func.closure_scope));

            // For each parameter, either use the provided argument or default to Nil
            for (idx, name) in func.params.iter().enumerate() {
                let val = arg_values.get(idx).cloned().unwrap_or(Val::Nil);
                func_scope.declare(name, val);
            }

            match exec_stmt(Rc::new(func_scope), &func.body)? {
                ExecResult::Return(val) => Ok(val),
                ExecResult::Void => Ok(Val::Nil),
            }
        }
    }
}

fn eval_bin_op(scope: &Rc<Scope>, left: &Expr, op: BinOp, right: &Expr) -> Result<Val, ExecError> {
    let left_val = eval_expr(scope, left)?;
    let right_val = eval_expr(scope, right)?;

    match (op, left_val, right_val) {
        (BinOp::Add, Val::Num(l), Val::Num(r)) => Ok(Val::Num(l + r)),

        (BinOp::Sub, Val::Num(l), Val::Num(r)) => Ok(Val::Num(l - r)),

        (BinOp::Mul, Val::Num(l), Val::Num(r)) => Ok(Val::Num(l * r)),

        (BinOp::Div, Val::Num(l), Val::Num(r)) => {
            if r == 0.0 {
                return Err(ExecError::DivByZero);
            }
            Ok(Val::Num(l / r))
        }

        (BinOp::Eq, l, r) => Ok(Val::Bool(l == r)),
        (BinOp::NotEq, l, r) => Ok(Val::Bool(l != r)),

        (BinOp::Concat, Val::Str(l), Val::Str(r)) => Ok(Val::Str(format!("{l}{r}"))),

        (op, l, r) => Err(ExecError::InvalidBinaryOp {
            op,
            left: l.type_of(),
            right: r.type_of(),
        }),
    }
}

fn exec_assign(scope: &Rc<Scope>, place: &Expr, expr: &Expr) -> Result<Val, ExecError> {
    let Expr::Var(name) = place else {
        return Err(ExecError::InvalidAssignment);
    };
    let val = eval_expr(scope, expr)?;

    if !scope.assign(name, val.clone()) {
        return Err(ExecError::UndefVar {
            name: name.to_string(),
        });
    }

    Ok(val)
}

fn eval_unary_op(scope: &Rc<Scope>, op: UnaryOp, expr: &Expr) -> Result<Val, ExecError> {
    let val = eval_expr(scope, expr)?;
    match (op, val) {
        (UnaryOp::Pos, Val::Num(n)) => Ok(Val::Num(n)),
        (UnaryOp::Neg, Val::Num(n)) => Ok(Val::Num(-n)),
        (UnaryOp::Not, Val::Bool(b)) => Ok(Val::Bool(!b)),
        (op, v) => Err(ExecError::InvalidUnaryOp {
            op,
            ty: v.type_of(),
        }),
    }
}

#[cfg(test)]
#[path = "vm.tests.rs"]
mod tests;
