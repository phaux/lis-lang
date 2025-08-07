use std::cell::RefCell;
use std::collections::HashMap;

use std::rc::Rc;

#[derive(Debug)]
pub struct Scope {
    pub vars: RefCell<HashMap<String, Val>>,
    pub parent: Option<Rc<Scope>>,
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
    pub(crate) props: HashMap<String, Val>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Prim {
    /// A special value that represents the absence of a value.
    Nil,
    Num(f64),
    Bool(bool),
    Str(String),
}
