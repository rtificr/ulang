use std::collections::HashMap;

use string_interner::StringInterner;

use crate::{
    StringInt, TypeReg,
    ast::{StringId, TypeId},
    runtime::value::Value,
};

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub enum Type {
    #[default]
    Nil,
    Number,
    String,
    Boolean,
    Tuple(Vec<TypeId>),
    Union(Vec<TypeId>),
    Array(TypeId),
    Object(Vec<(StringId, TypeId)>),
    Function {
        params: Vec<TypeId>,
        return_type: TypeId,
    },
    Any,
    Type,
    Module,
    Builtin,
    Reference(TypeId),
    TBD,
}
impl Type {
    pub fn to_string(&self, strint: &StringInt, typereg: &TypeReg) -> String {
        match self {
            Type::Nil => "nil".into(),
            Type::Number => "number".into(),
            Type::String => "string".into(),
            Type::Boolean => "boolean".into(),
            Type::Tuple(types) => format!(
                "tuple<{}>",
                types
                    .iter()
                    .map(|ty| format!(
                        "{}",
                        typereg
                            .resolve(*ty)
                            .unwrap_or(&Type::Any)
                            .to_string(strint, typereg)
                    ))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Type::Union(types) => format!(
                "union<{}>",
                types
                    .iter()
                    .map(|ty| format!(
                        "{}",
                        typereg
                            .resolve(*ty)
                            .unwrap_or(&Type::Any)
                            .to_string(strint, typereg)
                    ))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Type::Array(of) => format!(
                "array<{}>",
                typereg
                    .resolve(*of)
                    .unwrap_or(&Type::Any)
                    .to_string(strint, typereg)
            ),
            Type::Object(fields) => format!(
                "object<{{{}}}>",
                fields
                    .iter()
                    .map(|(name, ty)| format!(
                        "{}: {}",
                        strint.resolve(*name).unwrap_or(&"unknown".to_string()),
                        typereg
                            .resolve(*ty)
                            .unwrap_or(&Type::Any)
                            .to_string(strint, typereg)
                    ))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Type::Function {
                params,
                return_type,
            } => format!(
                "function({}) -> {}",
                params
                    .iter()
                    .map(|ty| format!(
                        "{}",
                        typereg
                            .resolve(*ty)
                            .unwrap_or(&Type::Any)
                            .to_string(strint, typereg)
                    ))
                    .collect::<Vec<_>>()
                    .join(", "),
                typereg
                    .resolve(*return_type)
                    .unwrap_or(&Type::Any)
                    .to_string(strint, typereg)
            ),
            Type::Any => "any".into(),
            Type::Type => "type".into(),
            Type::Module => "module".into(),
            Type::Builtin => "builtin".into(),
            Type::Reference(ty) => format!(
                "&{}",
                typereg
                    .resolve(*ty)
                    .unwrap_or(&Type::Any)
                    .to_string(strint, typereg)
            ),
            Type::TBD => "tbd".into(),
        }
    }
    pub fn merge(a: &TypeId, b: &TypeId, typereg: &mut TypeReg) -> TypeId {
        if a == b {
            return *a;
        }
        let type_a = typereg.resolve(*a).unwrap_or(&Type::Any).clone();
        let type_b = typereg.resolve(*b).unwrap_or(&Type::Any).clone();
        match (type_a, type_b) {
            (Type::Any, _) => *a,
            (_, Type::Any) => *b,
            (Type::Union(mut types_a), Type::Union(types_b)) => {
                for ty in types_b {
                    if !types_a.contains(&ty) {
                        types_a.push(ty);
                    }
                }
                typereg.get_or_intern(&Type::Union(types_a))
            }
            (Type::Union(mut types), ty) | (ty, Type::Union(mut types)) => {
                if !types.contains(&typereg.get_or_intern(&ty)) {
                    types.push(typereg.get_or_intern(&ty));
                }
                typereg.get_or_intern(&Type::Union(types))
            }
            (a, b) => {
                let id_a = typereg.get_or_intern(&a);
                let id_b = typereg.get_or_intern(&b);
                typereg.get_or_intern(&Type::Union(vec![id_a, id_b]))
            }
        }
    }
}

pub fn supports(a_id: &TypeId, b_id: &TypeId, typereg: &TypeReg) -> Option<bool> {
    if a_id == b_id {
        return Some(true);
    }
    let a = typereg.resolve(*a_id)?;
    let b = typereg.resolve(*b_id)?;
    match (a, b) {
        (Type::Any, _) => Some(true),
        (_, Type::Any) => Some(false),
        (Type::Union(types_a), Type::Union(types_b)) => {
            let all_a_in_b = types_a.iter().all(|ty| types_b.contains(ty));
            Some(all_a_in_b)
        }
        (Type::Union(types), _) => {
            for ty in types {
                if ty == b_id {
                    return Some(true);
                }
            }
            Some(false)
        }
        (_, Type::Union(types)) => {
            for ty in types {
                if ty == a_id {
                    return Some(true);
                }
            }
            Some(false)
        }
        (Type::Nil, Type::Nil) => Some(true),
        (Type::Number, Type::Number) => Some(true),
        (Type::String, Type::String) => Some(true),
        (Type::Boolean, Type::Boolean) => Some(true),
        (Type::Array(a), Type::Array(b)) => supports(a, b, typereg),
        (Type::Object(_), Type::Object(_)) => Some(a_id == b_id),
        (
            Type::Function {
                params: params_a,
                return_type: return_a,
            },
            Type::Function {
                params: params_b,
                return_type: return_b,
            },
        ) => Some(true),
        (Type::Type, Type::Type) => Some(true),
        (Type::Module, Type::Module) => Some(true),
        _ => Some(false),
    }
}
