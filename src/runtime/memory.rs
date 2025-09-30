use ahash::AHashMap as HashMap;
use slotmap::SlotMap;

use crate::{ast::StringId, runtime::value::Value, scopes::Scopes};

pub type ValueId = slotmap::DefaultKey;

pub struct Memory {
    pub values: SlotMap<ValueId, Value>,
    pub scope_stack: Vec<HashMap<StringId, ValueId>>,
}
impl Memory {
    pub fn new() -> Self {
        Self {
            values: SlotMap::with_key(),
            scope_stack: vec![HashMap::new()],
        }
    }
    pub fn alloc(&mut self, value: Value) -> ValueId {
        self.values.insert(value)
    }
    pub fn get(&self, id: ValueId) -> Option<&Value> {
        self.values.get(id)
    }
    pub fn get_mut(&mut self, id: ValueId) -> Option<&mut Value> {
        self.values.get_mut(id)
    }
}