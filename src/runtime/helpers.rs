use crate::{ast::{StringId, TypeId}, runtime::{memory::ValPtr, types::Type, value::Value, EvalReason, Evaluation, Runtime}};

impl Runtime {
    pub fn resolve_str(&self, id: StringId) -> Option<&str> {
        self.strint.resolve(id)
    }
    pub fn intern_str(&mut self, s: &str) -> StringId {
        self.strint.get_or_intern(s)
    }
    pub fn resolve_type(&self, id: TypeId) -> Option<&Type> {
        self.typereg.resolve(id)
    }
    pub fn alloc_type(&mut self, ty: &Type) -> TypeId {
        self.typereg.get_or_intern(ty)
    }
    pub fn resolve_typename(&self, id: TypeId) -> String {
        if let Some(ty) = self.resolve_type(id) {
            ty.to_string(&self.strint, &self.typereg)
        } else {
            "unknown".into()
        }
    }

    pub fn resolve_value(&self, id: ValPtr) -> Option<&Value> {
        self.memory.get(id)
    }
    pub fn malloc(&mut self, value: Value) -> ValPtr {
        self.memory.malloc(value)
    }
    pub fn malloc_eval(&mut self, value: Value) -> Evaluation {
        self.malloc(value).into()
    }
    pub fn resolve_value_str(&self, id: ValPtr) -> Option<String> {
        self.memory.get(id).map(|v| self.value_to_string(v)).transpose().ok().flatten()
    }
}