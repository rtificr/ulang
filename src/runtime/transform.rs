use crate::{
    ast::TypeId,
    runtime::{Runtime, memory::ValPtr, types::Type, value::Value},
};
use anyhow::*;
use std::vec;

impl Runtime {
    pub fn get_val_typeid(&mut self, value: Value) -> Result<TypeId> {
        let ty = self.get_val_type(value)?;
        Ok(TypeId::from_raw(self.typereg.get_or_intern(&ty)))
    }
    pub fn get_val_type(&mut self, value: Value) -> Result<Type> {
        Ok(match value {
            Value::Nil => Type::Nil,
            Value::Number(_) => Type::Number,
            Value::String(_) => Type::String,
            Value::Boolean(_) => Type::Boolean,
            Value::Tuple(elements) => Type::Tuple(
                elements
                    .iter()
                    .map(|v| self.get_val_typeid(self.memory.err_get(*v)?.clone()))
                    .collect::<Result<Vec<_>>>()?,
            ),
            Value::Array { of, .. } => Type::Array(of),
            Value::Table(_) => Type::Table,
            Value::Function {
                params,
                return_type,
                ..
            } => Type::Function {
                params: params.iter().map(|(_, ty)| *ty).collect(),
                return_type: return_type,
            },
            Value::Builtin(_) => Type::Builtin,
            Value::Module(_) => Type::Module,
            Value::Type(type_id) => {
                if let Some(ty) = self.resolve_type(type_id) {
                    ty.clone()
                } else {
                    Type::Any
                }
            }
            Value::Reference(value_id) => {
                if let Some(v) = self.resolve_value(value_id).cloned() {
                    let typeid = self.get_val_typeid(v)?;
                    Type::Reference(typeid)
                } else {
                    Type::Any
                }
            }
        })
    }
    pub fn value_to_string(&self, value: &Value) -> Result<String> {
        Ok(match value {
            Value::Nil => "nil".into(),
            Value::Number(n) => n.to_string(),
            Value::String(s) => self.resolve_str(*s).unwrap().to_string(),
            Value::Boolean(b) => b.to_string(),
            Value::Tuple(elements) => format!(
                "({})",
                elements
                    .iter()
                    .map(|v| self.value_to_string(self.memory.err_get(*v)?))
                    .collect::<Result<Vec<_>>>()?
                    .join(", ")
            ),
            Value::Array { of, elements } => format!(
                "[{}]",
                elements
                    .iter()
                    .map(|v| self.value_to_string(self.memory.err_get(*v)?))
                    .collect::<Result<Vec<_>>>()?
                    .join(", ")
            ),
            Value::Table(map) => format!(
                "{{{}}}",
                map.inner()
                    .iter()
                    .map(|(k, v)| {
                        Ok(format!(
                            "{}: {}",
                            self.value_to_string(self.memory.err_get(*v)?)?,
                            self.value_to_string(self.memory.err_get(*v)?)?
                        ))
                    })
                    .collect::<Result<Vec<_>>>()?
                    .join(", ")
            ),
            Value::Function {
                params,
                body,
                return_type,
            } => format!(
                "<function {:?} -> {:?}>",
                params
                    .iter()
                    .map(|(name, ty)| format!(
                        "{}: {:?}",
                        self.strint.resolve(name.raw()).unwrap(),
                        self.typereg.resolve(ty.raw()).unwrap_or(&Type::Any)
                    ))
                    .collect::<Vec<_>>()
                    .join(", "),
                self.typereg
                    .resolve(return_type.raw())
                    .unwrap_or(&Type::Any)
            ),
            Value::Builtin(f) => format!("<builtin fn {:?}>", f),
            Value::Module(path_id) => {
                format!(
                    "<module {:?}>",
                    self.resolve_str(*path_id).unwrap_or("unknown")
                )
            }
            Value::Type(type_id) => format!("<type {:?}>", self.resolve_typename(*type_id)),
            Value::Reference(value_id) => format!("<& {:?}>", self.resolve_value_str(*value_id)),
        })
    }

    pub fn derive_type(&mut self, values: &[ValPtr]) -> Result<TypeId> {
        let Some(init) = values.first() else {
            return Ok(TypeId::from_raw(self.typereg.get_or_intern(&Type::TBD)));
        };
        let ival = self.memory.err_get(*init)?.clone();
        let mut init = self.get_val_typeid(ival)?;
        for value in values.iter().skip(1) {
            let v = self.memory.err_get(*value)?.clone();
            let id = &self.get_val_typeid(v)?;
            init = self.merge_types(&init, id);
        }
        Ok(init)
    }

    pub fn merge_types(&mut self, a_id: &TypeId, b_id: &TypeId) -> TypeId {
        if a_id == b_id {
            return *a_id;
        }
        let a = self
            .typereg
            .resolve(a_id.raw())
            .unwrap_or(&Type::Any)
            .clone();
        let b = self
            .typereg
            .resolve(b_id.raw())
            .unwrap_or(&Type::Any)
            .clone();
        match (a, b) {
            (Type::Any, _) => *a_id,
            (_, Type::Any) => *b_id,
            (Type::Union(_), Type::Union(_)) => self.alloc_type(&Type::Union(vec![*a_id, *b_id])),
            (Type::Union(mut types), _) => {
                if !types.contains(&b_id) {
                    types.push(b_id.clone());
                }
                self.alloc_type(&Type::Union(types))
            }
            (_, Type::Union(mut types)) => {
                if !types.contains(&a_id) {
                    types.push(a_id.clone());
                }
                self.alloc_type(&Type::Union(types))
            }
            (_, _) => {
                TypeId::from_raw(self.typereg.get_or_intern(&Type::Union(vec![*a_id, *b_id])))
            }
        }
    }

    pub fn interpret_as_type_literal(&mut self, value: &Value) -> Result<TypeId> {
        match value {
            Value::Type(type_id) => Ok(*type_id),
            Value::Array { of, .. } => Ok(self.alloc_type(&Type::Array(*of))),
            Value::Tuple(elements) => {
                let types = elements
                    .iter()
                    .map(|v| self.interpret_as_type_literal(&self.memory.err_get(*v)?.clone()))
                    .collect::<Result<Vec<_>>>()?;
                Ok(self.alloc_type(&Type::Tuple(types)))
            }
            Value::Reference(ptr) => {
                let inner = self.interpret_as_type_literal(&self.memory.err_get(*ptr)?.clone())?;
                Ok(self.alloc_type(&Type::Reference(inner)))
            }
            _ => bail!(
                "Cannot interpret value as type literal: {}",
                self.value_to_string(value)?
            ),
        }
    }
}
