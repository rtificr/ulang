use std::hash::Hash;
use anyhow::*;
use ahash::AHashMap as HashMap;
use nohash_hasher::IntMap;
use slotmap::SlotMap;

use crate::{ast::StringId, runtime::value::Value, scopes::Scopes};

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
    // Use the raw DefaultKey internally for slotmap operations. Public APIs
    // convert to/from `ValPtr` so callers don't see the raw key type.
    pub values: SlotMap<slotmap::DefaultKey, Value>,
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
        let k = self.values.insert(value);
        ValPtr::from(k)
    }
    pub fn malloc_mut(&mut self, value: Value) -> &mut Value {
        let id = self.values.insert(value);
        self.values.get_mut(id).unwrap()
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
        let v = self.values.get_mut(key).ok_or(anyhow!("Value ID not found in memory"))?;
        Ok(f(v))
    }

    pub fn push_scope(&mut self) {
        self.scope_stack.push(Scope::new());
    }
    pub fn push_exclusive(&mut self) {
        self.scope_stack.push(Scope::new_exclusive());
    }
    pub fn pop_scope(&mut self) {
        self.scope_stack.pop();
    }
    pub fn get(&self, id: ValPtr) -> Option<&Value> {
        let key: slotmap::DefaultKey = id.into();
        self.values.get(key)
    }
    pub fn err_get(&self, id: ValPtr) -> Result<&Value> {
        let key: slotmap::DefaultKey = id.into();
        let val = self.values.get(key).ok_or(anyhow!("Value ID not found in memory"))?;
        Ok(val)
    }
    pub fn err_get_mut(&mut self, id: ValPtr) -> Result<&mut Value> {
        let key: slotmap::DefaultKey = id.into();
        self.values.get_mut(key).ok_or(anyhow!("Value ID not found in memory"))
    }
    pub fn get_mut(&mut self, id: ValPtr) -> Option<&mut Value> {
        let key: slotmap::DefaultKey = id.into();
        self.values.get_mut(key)
    }
}

pub struct Scope {
    mode: ScopeMode,
    map: IntMap<usize, ScopeValue>
}
pub type ScopeKey = StringId;
pub type ScopeValue = ValPtr;
impl Scope {
    pub fn new() -> Self {
        Self {
            mode: ScopeMode::Inclusive,
            map: IntMap::default()
        }
    }
    pub fn new_exclusive() -> Self {
        Self {
            mode: ScopeMode::Exclusive,
            map: IntMap::default()
        }
    }
    pub fn insert(&mut self, key: ScopeKey, value: ScopeValue) {
        self.map.insert(key.0, value);
    }
    pub fn get(&self, key: &ScopeKey) -> Option<&ScopeValue> {
        self.map.get(&key.0)
    }
    pub fn get_mut(&mut self, key: &ScopeKey) -> Option<&mut ScopeValue> {
        self.map.get_mut(&key.0)
    }
}
pub enum ScopeMode {
    Exclusive,
    Inclusive
}