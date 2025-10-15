use crate::{
    StringInt, TypeReg,
    ast::{NodeId, Operator, StringId, TypeId, TypeIdent},
    runtime::{AccessMode, EvalReason, Evaluation, Runtime, memory::ValPtr, types::Type},
};
use anyhow::{Result, bail};
use std::{collections::HashMap, fmt::Display, hash::Hash};

pub type BuiltinFn = fn(&mut Runtime, Vec<Value>) -> Result<Value>;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    Number(f64),
    String(StringId),
    Boolean(bool),
    Array {
        of: TypeId,
        elements: Vec<ValPtr>,
    },
    Tuple(Vec<ValPtr>),
    Table(Table),
    Function {
        params: Vec<(StringId, TypeId)>,
        body: NodeId,
        return_type: TypeId,
    },
    Builtin(BuiltinFn),
    Module(StringId),
    Type(TypeId),
    Reference(ValPtr),
}
impl Eq for Value {}
impl Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
    }
}
impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Boolean(b) => *b,
            _ => false,
        }
    }
    pub fn to_eval(self, runtime: &mut Runtime) -> Evaluation {
        Evaluation::new(runtime.malloc(self))
    }
}

pub type TableKey = Value;
pub type TableValue = ValPtr;
pub type TableMap = HashMap<TableKey, TableValue>;
#[derive(Debug, Clone, PartialEq)]
pub struct Table {
    inner: TableMap
}
impl Table {
    pub fn new() -> Self {
        Self {
            inner: TableMap::new()
        }
    }
    pub fn insert(&mut self, key: TableKey, value: TableValue) -> Option<TableValue> {
        self.inner.insert(key, value)
    } 
    pub fn get(&self, key: &TableKey) -> Option<&TableValue> {
        self.inner.get(key)
    } 
    pub fn inner(&self) -> &TableMap {
        &self.inner
    }
}