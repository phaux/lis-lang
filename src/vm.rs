use std::{collections::HashMap, rc::Rc};

use thiserror::Error;

use crate::{
    ast::{BinOp, Expr, Pat, Prog, Stmt, UnaryOp},
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

    #[error("not a function: {0:?}")]
    NotAFunc(Type),

    #[error("cannot destructure non-object value")]
    InvalidDestructuring,
}

#[must_use]
#[derive(Debug, PartialEq)]
enum ExecResult {
    Void,
    Return(Val),
}

pub fn exec_prog(global_scope: Rc<Scope>, prog: &Prog) -> Result<Val, ExecError> {
    let prog_scope = Rc::new(Scope::new(global_scope));
    for stmt in &prog.stmts {
        match exec_stmt(Rc::clone(&prog_scope), stmt)? {
            ExecResult::Void => {}
            ExecResult::Return(val) => return Ok(val),
        }
    }
    Ok(Val::Nil)
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
        Stmt::Let { pat, expr } => {
            let val = eval_expr(&scope, expr)?;
            match_pattern(&scope, pat, val)?;
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
        Expr::Nil => Ok(Val::Nil),
        Expr::Bool(b) => Ok(Val::Bool(*b)),
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
            for (key, val) in props {
                let val = eval_expr(scope, val)?;
                obj.props.insert(key.clone(), val);
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
        Expr::FuncCall { func, args } => eval_func_call(scope, func, args),
    }
}

fn eval_func_call(scope: &Rc<Scope>, func: &Expr, args: &[Expr]) -> Result<Val, ExecError> {
    // Evaluate the callee
    let func = eval_expr(scope, func)?;
    let Val::Func(func) = func else {
        return Err(ExecError::NotAFunc(func.type_of()));
    };

    // Evaluate the arguments
    let mut arg_values = Vec::new();
    for arg in args {
        arg_values.push(eval_expr(scope, arg)?);
    }

    // Create a new scope for the function
    let func_scope = Scope::new(Rc::clone(&func.closure_scope));
    for (idx, name) in func.params.iter().enumerate() {
        // For each parameter, either use the provided argument or default to Nil
        let val = arg_values.get(idx).cloned().unwrap_or(Val::Nil);
        func_scope.declare(name, val);
    }

    // Execute the function body
    match exec_stmt(Rc::new(func_scope), &func.body)? {
        ExecResult::Return(val) => Ok(val),
        ExecResult::Void => Ok(Val::Nil),
    }
}

fn eval_bin_op(scope: &Rc<Scope>, left: &Expr, op: BinOp, right: &Expr) -> Result<Val, ExecError> {
    // Handle short-circuit evaluation for logical operators
    match op {
        BinOp::Or => {
            let left_val = eval_expr(scope, left)?;
            if let Val::Bool(true) = left_val {
                return Ok(Val::Bool(true));
            }
            let right_val = eval_expr(scope, right)?;
            match (left_val, right_val) {
                (Val::Bool(l), Val::Bool(r)) => Ok(Val::Bool(l || r)),
                (l, r) => Err(ExecError::InvalidBinaryOp {
                    op,
                    left: l.type_of(),
                    right: r.type_of(),
                }),
            }
        }
        BinOp::And => {
            let left_val = eval_expr(scope, left)?;
            if let Val::Bool(false) = left_val {
                return Ok(Val::Bool(false));
            }
            let right_val = eval_expr(scope, right)?;
            match (left_val, right_val) {
                (Val::Bool(l), Val::Bool(r)) => Ok(Val::Bool(l && r)),
                (l, r) => Err(ExecError::InvalidBinaryOp {
                    op,
                    left: l.type_of(),
                    right: r.type_of(),
                }),
            }
        }
        _ => {
            // For non-short-circuiting operators, evaluate both sides first
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

fn match_pattern(scope: &Rc<Scope>, pat: &Pat, matched_val: Val) -> Result<(), ExecError> {
    match pat {
        Pat::Ident(name) => {
            // Identifier pattern binds the matched value to a variable
            scope.declare(name.as_str(), matched_val);
        }
        Pat::Obj { props } => {
            // Object pattern matches each property of the matched value
            let Val::Obj(matched_obj) = matched_val else {
                return Err(ExecError::InvalidDestructuring);
            };
            for (prop_name, prop_pat) in props {
                let val = matched_obj
                    .props
                    .get(prop_name)
                    .cloned()
                    .unwrap_or(Val::Nil);
                match_pattern(scope, prop_pat, val)?;
            }
        }
        Pat::Default { pat, default } => {
            // Default pattern puts a default value in place of a nil matched value
            let matched_val = match matched_val {
                Val::Nil => eval_expr(scope, default)?,
                val => val,
            };
            match_pattern(scope, pat, matched_val)?;
        }
    }
    Ok(())
}

fn eval_unary_op(scope: &Rc<Scope>, op: UnaryOp, expr: &Expr) -> Result<Val, ExecError> {
    let val = eval_expr(scope, expr)?;
    match (op, val) {
        (UnaryOp::Pos, Val::Num(n)) => Ok(Val::Num(n)),
        (UnaryOp::Neg, Val::Num(n)) => Ok(Val::Num(-n)),
        (UnaryOp::Not, Val::Bool(b)) => Ok(Val::Bool(!b)),
        (op, val) => Err(ExecError::InvalidUnaryOp {
            op,
            ty: val.type_of(),
        }),
    }
}

#[cfg(test)]
#[path = "vm.tests.rs"]
mod tests;
