use std::hash::Hash;
use anyhow::*;
use ahash::AHashMap as HashMap;
use slotmap::SlotMap;

use crate::{ast::StringId, runtime::value::Value, scopes::Scopes};

pub type ValPtr = slotmap::DefaultKey;

pub struct Memory {
    pub values: SlotMap<ValPtr, Value>,
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
        self.values.insert(value)
    }
    pub fn malloc_mut(&mut self, value: Value) -> &mut Value {
        let id = self.values.insert(value);
        self.values.get_mut(id).unwrap()
    }
    pub fn named_malloc(&mut self, name: StringId, value: Value) -> ValPtr {
        let id = self.malloc(value);
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.insert(name, id);
        }
        id
    }
    pub fn attrib(&mut self, name: StringId, id: ValPtr) {
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.insert(name, id);
        }
    }
    pub fn search(&self, name: StringId) -> Option<ValPtr> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(id) = scope.get(&name) {
                return Some(*id);
            }
        }
        None
    }
    pub fn search_val(&self, name: StringId) -> Option<&Value> {
        self.get(self.search(name)?)
    }
    pub fn search_ref(&self, name: StringId) -> Option<Value> {
        Some(Value::Reference(self.search(name)?))
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
        self.values.get(id)
    }
    pub fn err_get(&self, id: ValPtr) -> Result<&Value> {
        let val = self.values.get(id).ok_or(anyhow!("Value ID not found in memory"))?;
        Ok(val)
    }
    pub fn err_get_mut(&mut self, id: ValPtr) -> Result<&mut Value> {
        self.values.get_mut(id).ok_or(anyhow!("Value ID not found in memory"))
    }
    pub fn get_mut(&mut self, id: ValPtr) -> Option<&mut Value> {
        self.values.get_mut(id)
    }
}

pub struct Scope {
    mode: ScopeMode,
    map: HashMap<ScopeKey, ScopeValue>
}
pub type ScopeKey = StringId;
pub type ScopeValue = ValPtr;
impl Scope {
    pub fn new() -> Self {
        Self {
            mode: ScopeMode::Inclusive,
            map: HashMap::new()
        }
    }
    pub fn new_exclusive() -> Self {
        Self {
            mode: ScopeMode::Exclusive,
            map: HashMap::new()
        }
    }
    pub fn insert(&mut self, key: ScopeKey, value: ScopeValue) {
        self.map.insert(key, value);
    }
    pub fn get(&self, key: &ScopeKey) -> Option<&ScopeValue> {
        self.map.get(key)
    }
    pub fn get_mut(&mut self, key: &ScopeKey) -> Option<&mut ScopeValue> {
        self.map.get_mut(key)
    }
}
pub enum ScopeMode {
    Exclusive,
    Inclusive
}