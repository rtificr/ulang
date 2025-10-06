use crate::{
    ast::{NodeId, Operator, StringId, TypeId, TypeIdent}, runtime::{memory::ValPtr, types::Type, EvalReason, Evaluation, Runtime, AccessMode}, StringInt, TypeReg
};
use anyhow::{bail, Result};
use std::{collections::HashMap, fmt::Display};

pub type BuiltinFn = fn(&mut Runtime, Vec<Value>) -> Result<Value>;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    Number(f64),
    String(String),
    Boolean(bool),
    Array {
        of: TypeId,
        elements: Vec<ValPtr>,
    },
    Tuple(Vec<ValPtr>),
    Object(HashMap<StringId, ValPtr>),
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