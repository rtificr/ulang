use crate::{
    ast::{NodeId, Operator, StringId, TypeId, TypeIdent}, runtime::{memory::ValueId, types::Type, Runtime}, StringInt, TypeReg
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
        elements: Vec<Value>,
    },
    Tuple(Vec<Value>),
    Object(HashMap<StringId, Value>),
    Function {
        params: Vec<(StringId, TypeId)>,
        body: NodeId,
        return_type: TypeId,
    },
    Builtin(BuiltinFn),
    Module(StringId),
    Type(TypeId),
    Reference(ValueId),
}
impl Value {
    pub fn to_string(&self, strint: &StringInt, typereg: &TypeReg) -> String {
        match self {
            Value::Nil => "nil".into(),
            Value::Number(n) => n.to_string(),
            Value::String(s) => s.clone(),
            Value::Boolean(b) => b.to_string(),
            Value::Array { of, elements } => format!(
                "[{}]",
                elements
                    .iter()
                    .map(|e| e.to_string(strint, typereg))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Value::Tuple(elements) => format!(
                "({})",
                elements
                    .iter()
                    .map(|e| e.to_string(strint, typereg))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Value::Object(map) => format!("object {:?}", map),
            Value::Function {
                params,
                body,
                return_type,
            } => format!(
                "function(params: {:?}, body: {}, return_type: {:?})",
                params, body, return_type
            ),
            Value::Builtin(b) => format!("builtin {}", *b as usize),
            Value::Module(node_id) => format!("module {}", node_id),
            Value::Type(type_id) => format!(
                "type {}",
                typereg
                    .resolve(*type_id)
                    .unwrap_or(&Type::Any)
                    .to_string(strint, typereg)
            ),
            Value::Reference(value_id) => format!("&{:?}", value_id),
        }
    }
    pub fn get_type(&self, strint: &mut StringInt, typereg: &mut TypeReg) -> TypeId {
        match self {
            Value::Nil => typereg.get_or_intern(&Type::Nil),
            Value::Number(_) => typereg.get_or_intern(&Type::Number),
            Value::String(_) => typereg.get_or_intern(&Type::String),
            Value::Boolean(_) => typereg.get_or_intern(&Type::Boolean),
            Value::Array { of, .. } => typereg.get_or_intern(&Type::Array(*of)),
            Value::Object(map) => {
                let mut fields = vec![];
                for (key, value) in map {
                    let key_id = strint.get_or_intern(key);
                    let value_type = value.get_type(strint, typereg);
                    fields.push((key_id, value_type));
                }
                typereg.get_or_intern(&Type::Object(fields))
            }
            Value::Tuple(elements) => {
                let element_types = elements
                    .iter()
                    .map(|e| e.get_type(strint, typereg))
                    .collect();
                typereg.get_or_intern(&Type::Tuple(element_types))
            }
            Value::Function {
                params,
                body: _,
                return_type,
            } => typereg.get_or_intern(&Type::Function {
                params: params.clone().iter().map(|(_, typ)| *typ).collect(),
                return_type: *return_type,
            }),
            Value::Type(_) => typereg.get_or_intern(&Type::Type),
            Value::Module(_) => typereg.get_or_intern(&Type::Module),
            Value::Builtin(_) => typereg.get_or_intern(&Type::Builtin),
            Value::Reference(value_id) => typereg.get_or_intern(&Type::Reference(self.get_type(strint, typereg))),
        }
    }
    // for making sure arraytype(type(number)) is interpreted as that and not an arrayvalue(value(type(number)))
    pub fn typify(&self, strint: &mut StringInt, typereg: &mut TypeReg) -> Result<TypeId> {
        match self {
            Value::Type(type_id) => Ok(*type_id),
            Value::Array { of, .. } if of == &typereg.get_or_intern(&Type::Type) => {
                Ok(typereg.get_or_intern(&Type::Array(of.clone())))
            }
            Value::Nil => Ok(typereg.get_or_intern(&Type::Nil)),
            Value::Tuple(elements) => {
                let element_types: Result<Vec<TypeId>> = elements
                    .iter()
                    .map(|e| e.typify(strint, typereg))
                    .collect();
                Ok(typereg.get_or_intern(&Type::Tuple(element_types?)))
            }
            _ => bail!("Cannot interpret value as type: {}", self.to_string(strint, typereg)),
        }
    }
    pub fn operate(
        &self,
        other: &Value,
        op: &Operator,
        strint: &mut StringInt,
        typereg: &mut TypeReg,
    ) -> Result<Value, String> {
        match (self, other) {
            (Value::String(a), b) if *op == Operator::Add => Ok(Value::String(format!(
                "{}{}",
                a,
                b.to_string(strint, typereg)
            ))),
            (Value::String(a), b)
                if *op == Operator::Multiply
                    && matches!(b, Value::Number(n) if *n >= 0.0 && n.fract() == 0.0) =>
            {
                let n = if let Value::Number(n) = b {
                    *n as usize
                } else {
                    0
                };
                Ok(Value::String(a.repeat(n)))
            }
            (a, Value::String(b)) if *op == Operator::Add => Ok(Value::String(format!(
                "{}{}",
                a.to_string(strint, typereg),
                b
            ))),
            (Value::Number(a), Value::Number(b)) => match op {
                Operator::Add => Ok(Value::Number(a + b)),
                Operator::Subtract => Ok(Value::Number(a - b)),
                Operator::Multiply => Ok(Value::Number(a * b)),
                Operator::Divide => Ok(Value::Number(a / b)),
                Operator::Modulus => Ok(Value::Number(a % b)),
                Operator::Equal => Ok(Value::Boolean(a == b)),
                Operator::NotEqual => Ok(Value::Boolean(a != b)),
                Operator::GreaterThan => Ok(Value::Boolean(a > b)),
                Operator::LessThan => Ok(Value::Boolean(a < b)),
                Operator::GreaterThanOrEqual => Ok(Value::Boolean(a >= b)),
                Operator::LessThanOrEqual => Ok(Value::Boolean(a <= b)),
                Operator::BitAnd => Ok(Value::Number((*a as i64 & *b as i64) as f64)),
                Operator::BitOr => Ok(Value::Number((*a as i64 | *b as i64) as f64)),
                Operator::BitXor => Ok(Value::Number((*a as i64 ^ *b as i64) as f64)),
                Operator::Exponent => Ok(Value::Number(a.powf(*b))),
                _ => Err(format!("Unsupported operator {:?} for numbers", op)),
            },
            (Value::String(a), Value::String(b)) => match op {
                Operator::Add => Ok(Value::String(format!("{}{}", a, b))),
                Operator::Equal => Ok(Value::Boolean(a == b)),
                Operator::NotEqual => Ok(Value::Boolean(a != b)),
                _ => Err(format!("Unsupported operator {:?} for strings", op)),
            },
            (Value::Boolean(a), Value::Boolean(b)) => match op {
                Operator::And => Ok(Value::Boolean(*a && *b)),
                Operator::Or => Ok(Value::Boolean(*a || *b)),
                Operator::Equal => Ok(Value::Boolean(a == b)),
                Operator::NotEqual => Ok(Value::Boolean(a != b)),
                _ => Err(format!("Unsupported operator {:?} for booleans", op)),
            },
            (Value::Type(a), Value::Type(b)) => match op {
                Operator::Equal => Ok(Value::Boolean(a == b)),
                Operator::NotEqual => Ok(Value::Boolean(a != b)),
                Operator::BitOr => Ok(Value::Type(Type::merge(a, b, typereg))),
                _ => Err(format!("Unsupported operator {:?} for types", op)),
            },
            _ => Err(format!(
                "Type mismatch for operation: {:?} and {:?}",
                self, other
            )),
        }
    }
    pub fn unary_operate(&self, op: &crate::ast::Operator) -> Result<Value, String> {
        match (self, op) {
            (Value::Number(n), Operator::Subtract) => Ok(Value::Number(-n)),
            (Value::Boolean(b), Operator::Not) => Ok(Value::Boolean(!b)),
            _ => Err(format!(
                "Unsupported unary operation {:?} on {:?}",
                op, self
            )),
        }
    }
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Boolean(b) => *b,
            _ => false,
        }
    }
}
