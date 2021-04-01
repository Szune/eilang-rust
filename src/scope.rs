/*
 * eilang is an experimental programming language, this is its compiler and interpreter.
 * Copyright (C) 2021  Carl Erik Patrik Iwarson
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
use crate::values::Value;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Scope {
    vars: HashMap<String, Rc<Value>>,
    parent: Rc<RefCell<Option<Scope>>>,
    typ: ScopeType,
}

#[derive(Clone, PartialEq)]
pub enum ScopeType {
    Global,
    Function,
}

impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}

impl Scope {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            parent: Rc::new(RefCell::new(None)),
            typ: ScopeType::Global,
        }
    }

    pub fn with_parent(parent: Self) -> Self {
        Self {
            vars: HashMap::new(),
            parent: Rc::new(RefCell::new(Some(parent))),
            typ: ScopeType::Function,
        }
    }

    pub fn get_type(&self) -> ScopeType {
        self.typ.clone()
    }

    /// Define variable in current scope
    pub fn define_variable(&mut self, var: String, val: Rc<Value>) {
        self.vars
            .entry(var)
            .and_modify(|e| *e = Rc::clone(&val))
            .or_insert_with(|| Rc::clone(&val));
    }

    pub fn get_parent(self) -> Option<Scope> {
        Rc::try_unwrap(self.parent)
            .unwrap_or_else(|_| RefCell::new(None))
            .into_inner()
    }

    /// Set value of variable in its containing scope
    pub fn set_variable(&mut self, var: String, val: Rc<Value>) {
        if self.has_variable(&var) {
            self.vars.insert(var, val);
        } else {
            loop {
                let current = Rc::clone(&self.parent);

                let borrowed = &mut *current.borrow_mut();
                if let Some(ref mut borrowed) = borrowed {
                    if borrowed.has_variable(&var) {
                        borrowed.vars.insert(var, val);
                        break;
                    }
                } else {
                    panic!("Variable {} has not been defined yet.", &var);
                }
            }
        }
    }

    pub fn has_variable(&self, var: &str) -> bool {
        self.vars.contains_key(var)
    }

    pub fn get_variable(&self, var: &str) -> Rc<Value> {
        let variable = self.vars.get(var);
        if let Some(variable) = variable {
            return Rc::clone(variable);
        }

        // bit of a mess
        let parent = Rc::clone(&self.parent);
        let borrowed: &RefCell<Option<Scope>> = parent.borrow();
        let live = match *borrowed.borrow() {
            Some(ref s) => s.get_variable(var),
            None => panic!("Variable {} has not been defined yet.", &var),
        };
        live
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn scope_get_var_from_parent() {
        let mut parent_scope = Scope::new();
        parent_scope.define_variable("test".into(), Rc::new(Value::bool(true)));
        let child_scope = Scope::with_parent(parent_scope);

        let test_var = child_scope.get_variable("test");

        match *test_var {
            Value::Bool(b) => assert!(b),
            _ => unreachable!(),
        }
    }

    #[test]
    pub fn scope_get_local_var_that_also_exists_in_parent() {
        let mut parent_scope = Scope::new();
        parent_scope.define_variable("some_var".into(), Rc::new(Value::bool(false)));
        let mut child_scope = Scope::with_parent(parent_scope);
        child_scope.define_variable("some_var".into(), Rc::new(Value::bool(true)));

        let some_var = child_scope.get_variable("some_var");

        match *some_var {
            Value::Bool(b) => assert!(b),
            _ => unreachable!(),
        }
    }

    #[test]
    pub fn scope_get_local_var_that_only_exists_in_child() {
        let mut parent_scope = Scope::new();
        parent_scope.define_variable("some_var2".into(), Rc::new(Value::bool(false)));
        let mut child_scope = Scope::with_parent(parent_scope);
        child_scope.define_variable("some_var1".into(), Rc::new(Value::bool(true)));

        let some_var = child_scope.get_variable("some_var1");

        match *some_var {
            Value::Bool(b) => assert!(b),
            _ => unreachable!(),
        }
    }

    #[test]
    #[should_panic]
    pub fn scope_try_get_var_that_is_not_defined() {
        let mut parent_scope = Scope::new();
        parent_scope.define_variable("some_var".into(), Rc::new(Value::bool(false)));
        let mut child_scope = Scope::with_parent(parent_scope);
        child_scope.define_variable("some_var".into(), Rc::new(Value::bool(true)));

        let _ = child_scope.get_variable("not_defined");
    }
}
