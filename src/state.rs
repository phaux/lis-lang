//! This module defines types which represent the state of a running program.
//! The root of the state is the current [Scope].

use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{Debug, Formatter},
    rc::Rc,
};

use uuid::Uuid;

use crate::ast::{Span, Stmt};

/// A scope stores declared variables.
/// It also has a pointer to the parent scope, which is used for variable lookup.
#[derive(Debug)]
pub struct Scope {
    pub vars: RefCell<HashMap<String, Val>>,
    pub parent: Option<Rc<Scope>>,
}

impl Scope {
    /// Creates a new root scope.
    pub fn root() -> Self {
        Scope {
            vars: RefCell::new(HashMap::new()),
            parent: None,
        }
    }

    /// Creates a new child scope.
    pub fn new_child(self: &Rc<Self>) -> Self {
        Scope {
            vars: RefCell::new(HashMap::new()),
            parent: Some(Rc::clone(self)),
        }
    }

    /// Declare and initialize a variable in this scope.
    pub fn declare(&self, name: &str, val: Val) {
        self.vars.borrow_mut().insert(name.to_owned(), val);
    }

    /// Lookup and assign a variable in this scope or any parent scope.
    /// Returns `true` if the variable was found and assigned, `false` otherwise.
    pub fn assign(&self, name: &str, val: Val) -> bool {
        let mut current = Some(self);
        while let Some(scope) = current {
            if let Some(v) = scope.vars.borrow_mut().get_mut(name) {
                *v = val;
                return true;
            }
            current = scope.parent.as_deref();
        }
        false
    }

    /// Lookup and return variable value from this scope or any parent scope.
    pub fn lookup(&self, name: &str) -> Option<Val> {
        let mut current = Some(self);
        while let Some(scope) = current {
            if let Some(val) = scope.vars.borrow().get(name) {
                return Some(val.clone());
            }
            current = scope.parent.as_deref();
        }
        None
    }
}

/// Any runtime value.
#[derive(Debug, Clone, PartialEq, Default)]
pub enum Val {
    /// A special value that represents the absence of a value.
    #[default]
    Nil,
    Num(f64),
    Bool(bool),
    Str(String),
    Obj(Rc<Obj>),
    Func(Rc<Func>),
}

impl Val {
    #[must_use]
    pub const fn type_of(&self) -> Type {
        match self {
            Val::Nil => Type::Nil,
            Val::Bool(_) => Type::Bool,
            Val::Num(_) => Type::Num,
            Val::Str(_) => Type::Str,
            Val::Obj(_) => Type::Obj,
            Val::Func(_) => Type::Func,
        }
    }
}

/// The type of a [Val].
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Nil,
    Bool,
    Num,
    Str,
    Obj,
    Func,
}

/// An object value.
#[derive(Debug, Clone, PartialEq)]
pub struct Obj {
    pub props: HashMap<String, Val>,
}

/// A function value.
#[derive(Clone)]
pub struct Func {
    pub id: Uuid,
    pub params: Vec<String>,
    /// AST of the function body.
    pub body: Rc<Span<Stmt>>,
    /// Pointer to the scope in which the function was declared.
    pub closure_scope: Rc<Scope>,
}

impl Debug for Func {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Func")
            .field("id", &self.id)
            .finish_non_exhaustive()
    }
}

impl PartialEq for Func {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
