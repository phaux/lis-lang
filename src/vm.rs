use std::{collections::HashMap, rc::Rc};

use crate::{
    ast::{BinOp, Expr, Node, Pat, Prog, Stmt, UnaryOp},
    state::{Func, Obj, Scope, Type, Val},
};

#[must_use]
#[derive(Debug, PartialEq)]
pub enum ExecResult {
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

fn exec_stmt(scope: Rc<Scope>, stmt: &Node<Stmt>) -> Result<ExecResult, ExecError> {
    match &stmt.node {
        Stmt::Expr { expr } => {
            eval_expr(&scope, expr)?;
            Ok(ExecResult::Void)
        }
        Stmt::Block { stmts, .. } => {
            let block_scope = Rc::new(Scope::new(scope));
            for stmt in stmts {
                if let ExecResult::Return(val) = exec_stmt(Rc::clone(&block_scope), stmt)? {
                    return Ok(ExecResult::Return(val));
                }
            }
            Ok(ExecResult::Void)
        }
        Stmt::Let { pat, init, .. } => {
            let val = eval_expr(&scope, init)?;
            match_pattern(&scope, pat, val)?;
            Ok(ExecResult::Void)
        }
        Stmt::FuncDecl {
            name, params, body, ..
        } => {
            scope.declare(
                name,
                Val::Func(Rc::new(Func::new(
                    Rc::clone(&scope),
                    params.clone(),
                    Rc::new(*body.clone()),
                ))),
            );
            Ok(ExecResult::Void)
        }
        Stmt::Return { expr, .. } => {
            let val = if let Some(expr) = expr {
                eval_expr(&scope, expr)?
            } else {
                Val::Nil
            };
            Ok(ExecResult::Return(val))
        }
        Stmt::Print { expr, .. } => {
            let value = eval_expr(&scope, expr)?;
            println!("{value:?}");
            Ok(ExecResult::Void)
        }
        Stmt::If {
            condition,
            cons_branch,
            alt_branch,
            ..
        } => {
            let condition = eval_expr(&scope, condition)?;
            if let Val::Bool(condition) = condition {
                if condition {
                    exec_stmt(scope, cons_branch)
                } else if let Some(alt_branch) = alt_branch {
                    exec_stmt(scope, alt_branch)
                } else {
                    Ok(ExecResult::Void)
                }
            } else {
                Err(ExecError::InvalidCondition(condition.type_of()))
            }
        }
    }
}

pub fn eval_expr(scope: &Rc<Scope>, expr: &Node<Expr>) -> Result<Val, ExecError> {
    match &expr.node {
        Expr::Nil => Ok(Val::Nil),
        Expr::Bool { val } => Ok(Val::Bool(*val)),
        Expr::Num { val } => Ok(Val::Num(*val)),
        Expr::Str { val } => Ok(Val::Str(val.clone())),
        Expr::Var { name } => Ok(scope.lookup(name).unwrap_or_default()),
        Expr::Assign { place, expr, .. } => eval_assign(scope, place, expr),
        Expr::UnaryOp { op, expr, .. } => eval_unary_op(scope, *op, expr),
        Expr::BinOp {
            left, op, right, ..
        } => eval_bin_op(scope, left, *op, right),
        Expr::Obj { props, .. } => {
            let mut obj = Obj {
                props: HashMap::new(),
            };
            for (key, val) in props {
                let val = eval_expr(scope, val)?;
                obj.props.insert(key.clone(), val);
            }
            Ok(Val::Obj(Rc::new(obj)))
        }
        Expr::PropAccess { obj, prop, .. } => match eval_expr(scope, obj)? {
            Val::Obj(obj) => match obj.props.get(prop) {
                Some(value) => Ok(value.clone()),
                None => Ok(Val::Nil),
            },
            v => Err(ExecError::InvalidPropAccess(v.type_of())),
        },
        Expr::FuncCall { callee, args, .. } => eval_func_call(scope, callee, args),
    }
}

fn eval_func_call(
    scope: &Rc<Scope>,
    callee: &Node<Expr>,
    args: &[Node<Expr>],
) -> Result<Val, ExecError> {
    // Evaluate the callee
    let callee_val = eval_expr(scope, callee)?;
    let Val::Func(callee_val) = callee_val else {
        return Err(ExecError::InvalidCall(callee_val.type_of()));
    };

    // Evaluate the arguments
    let mut arg_vals = Vec::new();
    for arg in args {
        arg_vals.push(eval_expr(scope, arg)?);
    }

    // Create a new scope for the function
    let func_scope = Scope::new(Rc::clone(&callee_val.closure_scope));
    for (idx, name) in callee_val.params.iter().enumerate() {
        // For each parameter, either use the provided argument or default to Nil
        let val = arg_vals.get(idx).cloned().unwrap_or(Val::Nil);
        func_scope.declare(name, val);
    }

    // Execute the function body
    match exec_stmt(Rc::new(func_scope), &callee_val.body)? {
        ExecResult::Return(val) => Ok(val),
        ExecResult::Void => Ok(Val::Nil),
    }
}

fn eval_bin_op(
    scope: &Rc<Scope>,
    left: &Node<Expr>,
    op: BinOp,
    right: &Node<Expr>,
) -> Result<Val, ExecError> {
    // Handle short-circuit evaluation for logical operators
    match op {
        BinOp::Or => {
            let left_val = eval_expr(scope, left)?;
            if let Val::Bool(true) = left_val {
                return Ok(Val::Bool(true));
            }
            let right_val = eval_expr(scope, right)?;
            match (left_val, right_val) {
                (Val::Bool(l), Val::Bool(r)) => return Ok(Val::Bool(l || r)),
                (l, r) => {
                    return Err(ExecError::InvalidBinOp {
                        op,
                        left: l.type_of(),
                        right: r.type_of(),
                    });
                }
            }
        }
        BinOp::And => {
            let left_val = eval_expr(scope, left)?;
            if let Val::Bool(false) = left_val {
                return Ok(Val::Bool(false));
            }
            let right_val = eval_expr(scope, right)?;
            match (left_val, right_val) {
                (Val::Bool(l), Val::Bool(r)) => return Ok(Val::Bool(l && r)),
                (l, r) => {
                    return Err(ExecError::InvalidBinOp {
                        op,
                        left: l.type_of(),
                        right: r.type_of(),
                    });
                }
            }
        }
        _ => {}
    }

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
        (op, l, r) => Err(ExecError::InvalidBinOp {
            op,
            left: l.type_of(),
            right: r.type_of(),
        }),
    }
}

fn eval_unary_op(scope: &Rc<Scope>, op: UnaryOp, expr: &Node<Expr>) -> Result<Val, ExecError> {
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

fn eval_assign(scope: &Rc<Scope>, place: &Node<Expr>, expr: &Node<Expr>) -> Result<Val, ExecError> {
    let Expr::Var { name } = &place.node else {
        return Err(ExecError::InvalidAssign);
    };
    let val = eval_expr(scope, expr)?;
    if !scope.assign(name, val.clone()) {
        return Err(ExecError::UndefVar {
            name: name.to_string(),
        });
    }
    Ok(val)
}

pub fn match_pattern(
    scope: &Rc<Scope>,
    pat: &Node<Pat>,
    matched_val: Val,
) -> Result<(), ExecError> {
    match &pat.node {
        Pat::Ident { name } => {
            // Identifier pattern binds the matched value to a variable
            scope.declare(name.as_str(), matched_val);
        }
        Pat::Obj { props, .. } => {
            // Object pattern matches each property of the matched value
            let Val::Obj(matched_obj) = matched_val else {
                return Err(ExecError::InvalidMatchObj(matched_val.type_of()));
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
        Pat::Default { pat, default, .. } => {
            // Default pattern replaces the matched value if it's nil.
            let matched_val = match matched_val {
                Val::Nil => eval_expr(scope, default)?,
                val => val,
            };
            match_pattern(scope, pat, matched_val)?;
        }
    }
    Ok(())
}

/// Error which can happen during program execution.
#[derive(Debug, PartialEq)]
pub enum ExecError {
    DivByZero,
    InvalidCondition(Type),
    InvalidUnaryOp { op: UnaryOp, ty: Type },
    InvalidBinOp { op: BinOp, left: Type, right: Type },
    InvalidPropAccess(Type),
    UndefVar { name: String },
    InvalidAssign,
    InvalidCall(Type),
    InvalidMatchObj(Type),
}

impl std::fmt::Display for ExecError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ExecError::DivByZero => write!(f, "division by zero"),
            ExecError::InvalidCondition(ty) => write!(f, "unexpected {ty:?} in a condition"),
            ExecError::InvalidUnaryOp { op, ty } => write!(f, "invalid {op:?} on {ty:?} value"),
            ExecError::InvalidBinOp { op, left, right } => {
                write!(f, "invalid {op:?} between {left:?} and {right:?} values")
            }
            ExecError::InvalidPropAccess(ty) => {
                write!(f, "invalid property access on {ty:?} value")
            }
            ExecError::UndefVar { name } => write!(f, "undefined variable {name:?}"),
            ExecError::InvalidAssign => write!(f, "invalid left-hand side of assignment"),
            ExecError::InvalidCall(ty) => write!(f, "invalid call on {ty:?} value"),
            ExecError::InvalidMatchObj(ty) => {
                write!(f, "invalid object destructuring of {ty:?} value")
            }
        }
    }
}

impl std::error::Error for ExecError {}

#[cfg(test)]
#[path = "vm.tests.rs"]
mod tests;
