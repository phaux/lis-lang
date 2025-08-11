use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{Debug, Formatter},
    rc::Rc,
};

use uuid::Uuid;

use crate::ast::Stmt;

#[derive(Default)]
pub struct Scope {
    pub parent: Option<Rc<Scope>>,
    pub vars: RefCell<HashMap<String, Val>>,
}

impl Debug for Scope {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Scope")
            .field("parent", &self.parent)
            .finish_non_exhaustive()
    }
}

impl Scope {
    pub fn new(parent: Rc<Scope>) -> Self {
        Scope {
            vars: RefCell::new(HashMap::new()),
            parent: Some(parent),
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

/// The type of a value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Nil,
    Bool,
    Num,
    Str,
    Obj,
    Func,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Obj {
    pub props: HashMap<String, Val>,
}

#[derive(Clone)]
pub struct Func {
    pub id: Uuid,
    pub params: Vec<String>,
    pub body: Rc<Stmt>,
    pub closure_scope: Rc<Scope>,
}

impl Func {
    pub fn new(scope: Rc<Scope>, params: Vec<String>, body: Rc<Stmt>) -> Self {
        Self {
            id: Uuid::new_v4(),
            params,
            body,
            closure_scope: scope,
        }
    }
}

impl Debug for Func {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[fn {}]", self.id)
    }
}

impl PartialEq for Func {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
