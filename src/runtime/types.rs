use crate::{StringInt, TypeReg, ast::TypeId};

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
    Table,
    Object,
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
                            .resolve(ty.raw())
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
                            .resolve(ty.raw())
                            .unwrap_or(&Type::Any)
                            .to_string(strint, typereg)
                    ))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Type::Array(of) => format!(
                "array<{}>",
                typereg
                    .resolve(of.raw())
                    .unwrap_or(&Type::Any)
                    .to_string(strint, typereg)
            ),
            Type::Table => "table".to_string(),
            Type::Object => "object".to_string(),
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
                            .resolve(ty.raw())
                            .unwrap_or(&Type::Any)
                            .to_string(strint, typereg)
                    ))
                    .collect::<Vec<_>>()
                    .join(", "),
                typereg
                    .resolve(return_type.raw())
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
                    .resolve(ty.raw())
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
        let type_a = typereg.resolve(a.raw()).unwrap_or(&Type::Any).clone();
        let type_b = typereg.resolve(b.raw()).unwrap_or(&Type::Any).clone();
        match (type_a, type_b) {
            (Type::Any, _) => *a,
            (_, Type::Any) => *b,
            (Type::Union(mut types_a), Type::Union(types_b)) => {
                for ty in types_b {
                    if !types_a.contains(&ty) {
                        types_a.push(ty);
                    }
                }
                TypeId::from_raw(typereg.get_or_intern(&Type::Union(types_a)))
            }
            (Type::Union(mut types), ty) | (ty, Type::Union(mut types)) => {
                if !types.contains(&TypeId::from_raw(typereg.get_or_intern(&ty))) {
                    types.push(TypeId::from_raw(typereg.get_or_intern(&ty)));
                }
                TypeId::from_raw(typereg.get_or_intern(&Type::Union(types)))
            }
            (a, b) => {
                let id_a = TypeId::from_raw(typereg.get_or_intern(&a));
                let id_b = TypeId::from_raw(typereg.get_or_intern(&b));
                TypeId::from_raw(typereg.get_or_intern(&Type::Union(vec![id_a, id_b])))
            }
        }
    }
}
pub fn supports(a_id: &TypeId, b_id: &TypeId, typereg: &TypeReg) -> Option<bool> {
    match typereg.resolve(a_id.raw())? {
        Type::Any => return Some(true),
        Type::TBD => return Some(true),
        _ => {}
    }
    if a_id == b_id {
        return Some(true);
    }
    let a = typereg.resolve(a_id.raw())?;
    let b = typereg.resolve(b_id.raw())?;
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
        (Type::Array(a), Type::Array(b)) => supports(&a, &b, typereg),
        (Type::Table, Type::Table) => Some(a_id == b_id),
        (
            Type::Function {
                params: params_a,
                return_type: return_a,
            },
            Type::Function {
                params: params_b,
                return_type: return_b,
            },
        ) => Some(params_a == params_b && return_a == return_b),
        (Type::Type, Type::Type) => Some(true),
        (Type::Module, Type::Module) => Some(true),
        _ => Some(false),
    }
}
