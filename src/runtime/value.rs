use crate::{
    ast::{NodeId, StringId, TypeId},
    runtime::{
        Evaluation, Runtime,
        memory::{Scope, ValPtr},
    },
};
use ahash::AHashMap;
use anyhow::Result;
use std::{collections::HashMap, hash::Hash};

pub type BuiltinFn = fn(&mut Runtime, Vec<Value>) -> Result<Value>;

#[derive(Debug, Clone)]
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
    Table(Table<Value, ValPtr>),
    Object(Table<StringId, ValPtr>),
    Function {
        enclosed: Scope,
        params: Vec<(StringId, TypeId)>,
        body: NodeId,
        return_type: TypeId,
    },
    Builtin(BuiltinFn),
    Module(StringId),
    Type(TypeId),
    Reference(ValPtr),
}
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Boolean(l0), Self::Boolean(r0)) => l0 == r0,
            (
                Self::Array {
                    of: l_of,
                    elements: l_elements,
                },
                Self::Array {
                    of: r_of,
                    elements: r_elements,
                },
            ) => l_of == r_of && l_elements == r_elements,
            (Self::Tuple(l0), Self::Tuple(r0)) => l0 == r0,
            (Self::Table(l0), Self::Table(r0)) => l0 == r0,
            (
                Self::Function {
                    enclosed: l_enclosed,
                    params: l_params,
                    body: l_body,
                    return_type: l_return_type,
                },
                Self::Function {
                    enclosed: r_enclosed,
                    params: r_params,
                    body: r_body,
                    return_type: r_return_type,
                },
            ) => l_body == r_body,
            (Self::Builtin(l0), Self::Builtin(r0)) => l0 == r0,
            (Self::Module(l0), Self::Module(r0)) => l0 == r0,
            (Self::Type(l0), Self::Type(r0)) => l0 == r0,
            (Self::Reference(l0), Self::Reference(r0)) => l0 == r0,
            (Self::Nil, Self::Nil) => true,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
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

pub type FastMap<K, V> = AHashMap<K, V>;

#[derive(Debug, Clone, PartialEq)]
pub struct Table<K, V>
where K: Hash + Eq
{
    inner: FastMap<K, V>,
}
impl<K, V> Table<K, V>
where K: Hash + Eq
{
    pub fn new() -> Self {
        Self {
            inner: FastMap::new(),
        }
    }
    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        self.inner.insert(key, value)
    }
    pub fn get(&self, key: &K) -> Option<&V> {
        self.inner.get(key)
    }
    pub fn inner(&self) -> &FastMap<K, V> {
        &self.inner
    }
}
