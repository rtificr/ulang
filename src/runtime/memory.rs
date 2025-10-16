use anyhow::*;
use nohash_hasher::IntMap;
use slotmap::SlotMap;
use std::fmt::Debug;
use std::hash::Hash;

use crate::{ast::StringId, runtime::value::Value};

/// Opaque handle to a value stored in `Memory`.
/// Internally this wraps `slotmap::DefaultKey` but prevents callers from
/// accidentally using raw slotmap keys elsewhere in the codebase.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct ValPtr(pub slotmap::DefaultKey);

macro_rules! impl_ptr_helpers {
    ($name:ident, $raw:ty) => {
        impl From<$raw> for $name {
            fn from(k: $raw) -> Self {
                $name(k)
            }
        }
        impl From<$name> for $raw {
            fn from(v: $name) -> Self {
                v.0
            }
        }
        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}({:?})", stringify!($name), self.0)
            }
        }
        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{:?}", self.0)
            }
        }
    };
}
impl_ptr_helpers!(ValPtr, slotmap::DefaultKey);

pub struct Memory {
    pub values: SlotMap<slotmap::DefaultKey, (usize, Value)>,
    pub scope_stack: Vec<Scope>,
}

impl Memory {
    pub fn new() -> Self {
        Self {
            values: SlotMap::with_key(),
            scope_stack: vec![Scope::new()],
        }
    }
    pub fn alloc_global(&mut self, name: StringId, value: Value) -> ValPtr {
        let id = self.malloc(value);
        if let Some(scope) = self.scope_stack.first_mut() {
            scope.insert(name, id);
        }
        id
    }

    pub fn malloc(&mut self, value: Value) -> ValPtr {
        let k = self.values.insert((0, value));
        ValPtr::from(k)
    }
    pub fn malloc_mut(&mut self, value: Value) -> &mut Value {
        let key = self.values.insert((0, value));
        let entry = self.values.get_mut(key).unwrap();
        &mut entry.1
    }
    pub fn named_malloc(&mut self, name: StringId, value: Value) -> ValPtr {
        let id = self.malloc(value);
        self.attrib(name, id);
        id
    }
    /// Attribute a name to a pointer
    pub fn attrib(&mut self, name: StringId, id: ValPtr) {
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.insert(name, id);
        }
    }
    /// Return the raw `ValPtr` for a binding name (alias for `search`).
    /// This is often more ergonomic than matching on a `Value::Reference`.
    pub fn search_ptr(&self, name: StringId) -> Option<ValPtr> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(id) = scope.get(&name) {
                return Some(*id);
            }
            // If this scope is marked Exclusive then it hides outer scopes.
            // Stop searching further and return None to reflect exclusive semantics.
            if let ScopeMode::Exclusive = scope.mode {
                return None;
            }
        }
        None
    }
    pub fn search_val(&self, name: StringId) -> Option<&Value> {
        self.get(self.search_ptr(name)?)
    }

    /// Temporarily borrow a mutable reference to the `Value` at `id` and run `f`.
    /// Returns an error if the id is not present. This avoids returning long-lived
    /// `&mut Value` references from allocation helpers which can be accidentally
    /// held across other `Memory` mutations.
    pub fn with_mut<T, F>(&mut self, id: ValPtr, f: F) -> Result<T>
    where
        F: FnOnce(&mut Value) -> T,
    {
        let key: slotmap::DefaultKey = id.into();
        let v = self
            .values
            .get_mut(key)
            .ok_or(anyhow!("Value ID not found in memory"))?;
        Ok(f(&mut v.1))
    }

    pub fn push_new_scope(&mut self) {
        self.scope_stack.push(Scope::new());
    }
    pub fn push_scope(&mut self, scope: Scope) {
        self.scope_stack.push(scope)
    }
    pub fn push_new_exclusive_scope(&mut self) {
        self.scope_stack.push(Scope::new_exclusive());
    }
    pub fn push_exclusive_scope(&mut self, scope: Scope) {
        self.scope_stack.push(scope)
    }
    pub fn pop_scope(&mut self) {
        self.scope_stack.pop();
    }
    pub fn get(&self, id: ValPtr) -> Option<&Value> {
        let key: slotmap::DefaultKey = id.into();
        self.values.get(key).map(|p| &p.1)
    }
    pub fn err_get(&self, id: ValPtr) -> Result<&Value> {
        let key: slotmap::DefaultKey = id.into();
        let val = self
            .values
            .get(key)
            .map(|p| &p.1)
            .ok_or(anyhow!("Value ID not found in memory"))?;
        Ok(val)
    }
    pub fn err_get_mut(&mut self, id: ValPtr) -> Result<&mut Value> {
        let key: slotmap::DefaultKey = id.into();
        self.values
            .get_mut(key)
            .ok_or(anyhow!("Value ID not found in memory"))
            .map(|p| &mut p.1)
    }
    pub fn get_mut(&mut self, id: ValPtr) -> Option<&mut Value> {
        let key: slotmap::DefaultKey = id.into();
        self.values.get_mut(key).map(|p| &mut p.1)
    }
    pub fn free(&mut self, id: ValPtr) {
        if let Some((rc, _)) = self.values.get_mut(id.0) {
            *rc -= 1;
            if *rc == 0 {
                self.values.remove(id.0);
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Scope {
    mode: ScopeMode,
    inner: IntMap<usize, ScopeValue>,
}
pub type ScopeKey = StringId;
pub type ScopeValue = ValPtr;
impl Scope {
    pub fn new() -> Self {
        Self {
            mode: ScopeMode::Inclusive,
            inner: IntMap::default(),
        }
    }
    pub fn new_exclusive() -> Self {
        Self {
            mode: ScopeMode::Exclusive,
            inner: IntMap::default(),
        }
    }
    pub fn inner(&self) -> &IntMap<usize, ScopeValue> {
        &self.inner
    }
    pub fn insert(&mut self, key: ScopeKey, value: ScopeValue) {
        self.inner.insert(key.0, value);
    }
    pub fn get(&self, key: &ScopeKey) -> Option<&ScopeValue> {
        self.inner.get(&key.0)
    }
    pub fn get_mut(&mut self, key: &ScopeKey) -> Option<&mut ScopeValue> {
        self.inner.get_mut(&key.0)
    }
    pub fn merge(&mut self, other: Scope) {
        self.inner.extend(other.inner)
    }
    pub fn from_collapsed(scopes: &Vec<Scope>) -> Self {
        let mut s = Self::new();
        for scope in scopes {
            s.merge(scope.clone());
        }
        s
    }
}
#[derive(Clone, Copy, Debug)]
pub enum ScopeMode {
    Exclusive,
    Inclusive,
}
