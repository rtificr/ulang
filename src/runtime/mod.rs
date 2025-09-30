use core::str;
use std::collections::HashMap;

use anyhow::{anyhow, bail};

use crate::{
    NodeReg, StringInt, TypeReg,
    ast::{Literal, Node, NodeId, StringId, TypeId, TypeIdent},
    module::ModuleLoader,
    runtime::{
        types::{Type, derive_type, supports},
        value::{BuiltinFn, Value},
    },
    scopes::Scopes,
};

pub mod types;
pub mod value;

pub struct Runtime<'m> {
    pub nodes: NodeReg,
    pub strint: StringInt,
    pub typereg: TypeReg,
    pub scopes: Scopes<String, Value>,
    pub exports: HashMap<StringId, Value>,
    pub depth: usize,
    pub last_node_id: Option<NodeId>,
    pub modules: &'m mut ModuleLoader,
}
impl<'m> Runtime<'m> {
    pub fn new(
        nodes: NodeReg,
        strint: StringInt,
        typereg: TypeReg,
        modules: &'m mut ModuleLoader,
    ) -> Self {
        Self {
            nodes,
            strint,
            typereg,
            scopes: Scopes::new(),
            modules,
            exports: HashMap::new(),
            depth: 0,
            last_node_id: None,
        }
    }
    pub fn init_types(&mut self) {
        let basic_types = vec![
            ("nil", Type::Nil),
            ("number", Type::Number),
            ("string", Type::String),
            ("bool", Type::Boolean),
            ("any", Type::Any),
            ("type", Type::Type),
            ("module", Type::Module),
            ("builtin", Type::Builtin),
        ];
        for (name, typ) in basic_types {
            let type_id = self.typereg.get_or_intern(&typ);
            self.scopes
                .insert_global(name.to_string(), Value::Type(type_id));
        }
    }
    pub fn init_ministd(&mut self) {
        let supports = |runtime: &mut Runtime, args: Vec<Value>| -> anyhow::Result<Value> {
            if args.len() != 2 {
                bail!("supports expects 2 arguments, got {}", args.len());
            }
            let a = &args[0];
            let b = &args[1];
            let Value::Type(a_type_id) = a else {
                bail!("First argument to supports must be a Type");
            };
            let Value::Type(b_type_id) = b else {
                bail!("Second argument to supports must be a Type");
            };
            let result = types::supports(&a_type_id, &b_type_id, &runtime.typereg)
                .ok_or(anyhow!("Failed to resolve types for supports"))?;
            Ok(Value::Boolean(result))
        };
        self.scopes.insert_global(
            "supports".to_string(),
            Value::Builtin(supports as BuiltinFn),
        );

        fn print(runtime: &mut Runtime, args: Vec<Value>) -> anyhow::Result<Value> {
            println!(
                "{}",
                args.iter()
                    .map(|a| a.to_string(&runtime.strint, &runtime.typereg))
                    .collect::<Vec<_>>()
                    .join(" ")
            );
            Ok(Value::Nil)
        }
        self.scopes
            .insert_global("print".to_string(), Value::Builtin(print));

        fn typeof_(runtime: &mut Runtime, args: Vec<Value>) -> anyhow::Result<Value> {
            if args.len() != 1 {
                bail!("typeof expects 1 argument, got {}", args.len());
            }
            let value = &args[0];
            let type_id = value.get_type(&mut runtime.strint, &mut runtime.typereg);
            Ok(Value::Type(type_id))
        }
        self.scopes
            .insert_global("typeof".to_string(), Value::Builtin(typeof_ as BuiltinFn));

        fn sleep(runtime: &mut Runtime, args: Vec<Value>) -> anyhow::Result<Value> {
            if args.len() != 1 {
                bail!("sleep expects 1 argument, got {}", args.len());
            }
            let duration = &args[0];
            let Value::Number(n) = duration else {
                bail!("sleep expects a number argument");
            };
            std::thread::sleep(std::time::Duration::from_millis(*n as u64));
            Ok(Value::Nil)
        }
        self.scopes
            .insert_global("sleep".to_string(), Value::Builtin(sleep as BuiltinFn));
    }
    pub fn eval(&mut self, node_id: NodeId) -> anyhow::Result<Evaluation> {
        let node = self.nodes.get(node_id).cloned();
        self.last_node_id = Some(node_id);
        // let tabs = "  ".repeat(self.depth);
        // println!("{}Evaluating node_id {:?}: {:?}", tabs, node_id, node);
        self.depth += 1;
        let r = match node {
            Some(node) => match &node.node {
                Node::Export(expr) => {
                    let expr_value = self.eval(*expr)?;
                    if let Node::Declaration { name, .. } = &self.nodes.get(*expr).unwrap().node {
                        let name = self.strint.resolve(*name).unwrap().to_string();
                        self.exports
                            .insert(self.strint.get_or_intern(&name), expr_value.value.clone());
                        Ok(expr_value)
                    } else {
                        bail!("Can only export declarations");
                    }
                }
                Node::FieldAccess { object: object_id, field: field_id } => {
                    let object = self.eval(*object_id)?;
                    let field = self.strint.resolve(*field_id).unwrap().to_string();
                    let object = match &object.value {
                        Value::Object(map) => {
                            if let Some(value) = map.get(&field) {
                                value.clone()
                            } else {
                                bail!("Field '{}' not found in object", field);
                            }
                        }
                        Value::Module(path_id) => {
                            let path = self.strint.resolve(*path_id).unwrap().to_string();
                            let module = self.modules.load(&path, &mut self.strint)?;
                            if let Some(value) =
                                module.exports.get(field_id)
                            {
                                value.clone()
                            } else {
                                bail!("Field '{}' not found in module '{}'", field, path);
                            }
                        }
                        _ => bail!("Cannot access field '{}' on non-object value", field),
                    };
                    Ok(object.into())
                }
                Node::Import(path_id) => {
                    let path = self.strint.resolve(*path_id).unwrap().to_string();
                    let module = self.modules.load(&path, &mut self.strint)?;
                    for (export_name, export_value) in &module.exports {
                        self.scopes.insert(
                            self.strint.resolve(*export_name).unwrap().to_string(),
                            export_value.clone(),
                        );
                    }
                    Ok(Value::Nil.into())
                }
                Node::Literal(literal) => match literal {
                    Literal::Number(n) => Ok(Value::Number(*n).into()),
                    Literal::FString { parts } => {
                        let parts = parts
                            .iter()
                            .map(|nid| {
                                self.eval(*nid)
                                    .map(|eval| eval.value.to_string(&self.strint, &self.typereg))
                            })
                            .collect::<anyhow::Result<Vec<_>>>()?
                            .join("");
                        Ok(Value::String(parts).into())
                    }
                    Literal::String(s) => {
                        let s = self.strint.resolve(*s).unwrap();
                        Ok(Value::String(s.to_string()).into())
                    }
                    Literal::Boolean(b) => Ok(Value::Boolean(*b).into()),
                    Literal::Nil => Ok(Value::Nil.into()),
                    Literal::Array { elements } => {
                        let element_values = elements
                            .iter()
                            .map(|nid| self.eval(*nid).map(|eval| eval.value))
                            .collect::<anyhow::Result<Vec<_>>>()?;
                        Ok(Value::Array {
                            of: derive_type(&element_values, &mut self.typereg, &mut self.strint),
                            elements: element_values,
                        }
                        .into())
                    }
                    Literal::Tuple { elements } => {
                        let element_values = elements
                            .iter()
                            .map(|nid| self.eval(*nid).map(|eval| eval.value))
                            .collect::<anyhow::Result<Vec<_>>>()?;
                        Ok(Value::Array {
                            of: derive_type(&element_values, &mut self.typereg, &mut self.strint),
                            elements: element_values,
                        }
                        .into())
                    }
                },
                Node::Identifier(name_id) => {
                    let name: &str = self.strint.resolve(*name_id).unwrap();
                    if let Some(value) = self.scopes.get(&name.to_string()) {
                        Ok(value.clone().into())
                    } else {
                        if let Some(value) = self.exports.get(name_id) {
                            Ok(value.clone().into())
                        } else {
                            bail!("Undefined identifier: '{}'", name);
                        }
                    }
                }
                Node::TypeIdent(type_ident) => {
                    let base = self.strint.resolve(type_ident.base).unwrap();
                    let Some(mut t) = self.scopes.get(&base.to_string()).cloned() else {
                        bail!("Undefined type identifier: '{}'", base);
                    };
                    for _ in 0..type_ident.dims {
                        let type_id = match t {
                            Value::Type(id) => id,
                            _ => bail!("Expected type, found value in type identifier '{}'", base),
                        };
                        let array_type = types::Type::Array(type_id);
                        let array_type_id = self.typereg.get_or_intern(&array_type);
                        t = Value::Type(array_type_id);
                    }
                    Ok(t.into())
                }
                Node::IfExpr {
                    condition,
                    then_branch,
                    else_branch,
                } => {
                    let cond_value = self.eval(*condition)?.value.is_truthy();
                    if cond_value {
                        self.eval(*then_branch)
                    } else if let Some(else_branch) = else_branch {
                        self.eval(*else_branch)
                    } else {
                        Ok(Value::Nil.into())
                    }
                }
                Node::WhileExpr { condition, body } => {
                    let mut val = Value::Nil;
                    while self.eval(*condition)?.value.is_truthy() {
                        let eval = self.eval(*body)?;
                        match eval.reason {
                            EvalReason::Return => return Ok(eval),
                            EvalReason::Break => break,
                            EvalReason::Neither => {
                                val = eval.value;
                            }
                        }
                    }
                    Ok(val.into())
                }
                Node::ForExpr {
                    init: _,
                    condition: _,
                    update: _,
                    body: _,
                } => todo!(),
                Node::Declaration {
                    name,
                    ann,
                    value,
                    export,
                } => {
                    let name = self.strint.resolve(*name).unwrap().to_string();
                    let val = if let Some(value) = value {
                        self.eval(*value)?.value
                    } else {
                        Value::Nil
                    };
                    if let Some(type_node_id) = ann {
                        let ty = self
                            .eval(*type_node_id)?
                            .value
                            .typify(&mut self.strint, &mut self.typereg)?;
                        let expected_type_id = &self.typereg.get_or_intern(&Type::Type);
                        self.type_assert(&ty, expected_type_id)?;

                        if val.get_type(&mut self.strint, &mut self.typereg) != *expected_type_id {
                            let found_type = {
                                let id = val.get_type(&mut self.strint, &mut self.typereg);
                                self.typereg.resolve(id)
                            };
                            let expected_type = self.typereg.resolve(*expected_type_id);
                            bail!(
                                "Type mismatch in declaration of {}: expected {:?}, found {:?}",
                                name,
                                expected_type,
                                found_type,
                            );
                        }
                    }
                    match export {
                        true => {
                            self.exports
                                .insert(self.strint.get_or_intern(&name), val.clone());
                        }
                        false => self.scopes.insert(name.to_string(), val.clone()),
                    };
                    Ok(Value::Type(val.get_type(&mut self.strint, &mut self.typereg)).into())
                }
                Node::Assign { name, value } => {
                    let name = self.strint.resolve(*name).unwrap().to_string();
                    let val = self.eval(*value)?;
                    let entry = self
                        .scopes
                        .get_mut(&name)
                        .ok_or(anyhow!("Undefined identifier: {}", name))?;
                    *entry = val.value.clone();
                    Ok(val.into())
                }
                Node::Function { params, body } => {
                    let (resolved_params, unresolved_params): (Vec<_>, Vec<_>) = params
                        .iter()
                        .map(|p| ((p.name, self.resolve_type_ident(&p.type_))))
                        .partition(|(_, res)| res.is_ok());
                    let resolved_params: Vec<(StringId, TypeId)> = resolved_params
                        .into_iter()
                        .map(|(name, typ)| (name, typ.unwrap()))
                        .collect();
                    if !unresolved_params.is_empty() {
                        bail!(
                            "Failed to resolve parameter types: {:#?}",
                            unresolved_params
                                .into_iter()
                                .map(|(_, res)| res.err().unwrap().to_string())
                                .collect::<Vec<_>>()
                        );
                    }
                    Ok(Value::Function {
                        params: resolved_params,
                        body: body.clone(),
                        return_type: self.typereg.get_or_intern(&Type::Any),
                    }
                    .into())
                }
                Node::FunctionCall { name, args } => {
                    let func = {
                        let name = self
                            .strint
                            .resolve(*name)
                            .ok_or(anyhow!("Invalid function name"))?;
                        self.scopes
                            .get(&name.to_string())
                            .cloned()
                            .ok_or(anyhow!("Undefined function: {}", name))?
                    };

                    if let Value::Builtin(builtin) = func {
                        let arg_values: anyhow::Result<Vec<Value>> = args
                            .iter()
                            .map(|&arg| self.eval(arg).map(|e| e.value))
                            .collect();
                        let arg_values = arg_values?;
                        return builtin(self, arg_values).map(|v| v.into());
                    }

                    if let Value::Function {
                        params,
                        body,
                        return_type: _,
                    } = func
                    {
                        if params.len() != args.len() {
                            bail!(
                                "Function {} expects {} arguments, got {}",
                                name,
                                params.len(),
                                args.len()
                            );
                        }
                        let arg_values: anyhow::Result<Vec<Evaluation>> =
                            args.iter().map(|&arg| self.eval(arg)).collect();
                        let arg_values = arg_values?;

                        for (i, ((stringid, param_type_id), arg_value)) in
                            params.iter().zip(arg_values.iter()).enumerate()
                        {
                            let arg_type_id = arg_value
                                .value
                                .get_type(&mut self.strint, &mut self.typereg);
                            if supports(param_type_id, &arg_type_id, &self.typereg).unwrap_or(false)
                            {
                                bail!(
                                    "Type mismatch for argument {} in call to {}: expected {:?}, found {:?}",
                                    i + 1,
                                    self.strint.resolve(*name).unwrap_or("unknown"),
                                    self.typereg
                                        .resolve(*param_type_id)
                                        .map(|t| t.to_string(&self.strint, &self.typereg))
                                        .unwrap_or("unknown".to_string()),
                                    self.typereg
                                        .resolve(arg_type_id)
                                        .map(|t| t.to_string(&self.strint, &self.typereg))
                                        .unwrap_or("unknown".to_string())
                                );
                            }
                        }
                        self.scopes.push();
                        for (i, arg_value) in arg_values.into_iter().enumerate() {
                            let stringid = params[i].0;
                            let Some(name) = self.strint.resolve(stringid) else {
                                bail!("Invalid parameter name {}", stringid);
                            };
                            self.scopes.insert(name.to_string(), arg_value.into());
                        }
                        let result = self.eval(body);
                        self.scopes.pop();
                        result
                    } else {
                        bail!("{} is not a function", name)
                    }
                }
                Node::UnaryOp { op, expr } => {
                    let expr_value = self.eval(*expr)?;
                    let result = expr_value
                        .value
                        .unary_operate(&op)
                        .map_err(|e| anyhow!(e))?;
                    Ok(Evaluation {
                        value: result,
                        reason: expr_value.reason,
                    })
                }
                Node::BinaryOp { left, op, right } => {
                    let left_value = self.eval(*left)?;
                    let right_value = self.eval(*right)?;
                    let result = left_value
                        .value
                        .operate(&right_value.value, &op, &mut self.strint, &mut self.typereg)
                        .map_err(|e| anyhow!(e))?;
                    Ok(Evaluation {
                        value: result,
                        reason: EvalReason::Neither,
                    })
                }
                Node::Block { statements } => {
                    self.scopes.push();
                    let mut last_eval = Evaluation {
                        value: Value::Nil,
                        reason: EvalReason::Neither,
                    };
                    for stmt in statements {
                        last_eval = self.eval(*stmt)?;
                        // If we hit a return or break, stop executing statements
                        if matches!(last_eval.reason, EvalReason::Return | EvalReason::Break) {
                            break;
                        }
                    }
                    self.scopes.pop();
                    Ok(last_eval)
                }
                Node::IndexAccess { base: array, index } => {
                    let array_value = self.eval(*array)?;
                    let index_value = self.eval(*index)?;
                    let Value::Number(n) = index_value.value else {
                        bail!("Index must be a number");
                    };
                    let index = n as usize;
                    match &array_value.value {
                        Value::Array { of: _, elements } => {
                            if index >= elements.len() {
                                bail!("Index out of bounds");
                            }
                            Ok(elements[index].clone().into())
                        }
                        Value::Tuple(elements) => {
                            if index >= elements.len() {
                                bail!("Index out of bounds");
                            }
                            Ok(elements[index].clone().into())
                        }
                        _ => bail!("Cannot index into non-array value"),
                    }
                }
                Node::Return(value) => {
                    let value = match value {
                        Some(expr) => self.eval(*expr)?.value,
                        None => Value::Nil,
                    };
                    Ok(Evaluation {
                        value,
                        reason: EvalReason::Return,
                    })
                }
                Node::Break(_) => Ok(Evaluation {
                    value: Value::Nil,
                    reason: EvalReason::Break,
                }),
            },
            None => bail!("Node ID {:?} not found", node_id),
        };
        self.depth -= 1;
        // if let Ok(ref eval) = r {
        //     println!("{}Evaluation result for node_id {:?}: {:?}", tabs, node_id, eval.value);
        // } else if let Err(ref e) = r {
        //     println!("{}Evaluation error for node_id {:?}: {:?}", tabs, node_id, e);
        // }
        r
    }
    pub fn resolve_type_ident(&mut self, type_ident: &TypeIdent) -> anyhow::Result<TypeId> {
        let base = self.strint.resolve(type_ident.base).unwrap();
        let Some(mut t) = self.scopes.get(&base.to_string()).cloned() else {
            bail!("Undefined type identifier: {}", base);
        };
        for _ in 0..type_ident.dims {
            let type_id = t.typify(&mut self.strint, &mut self.typereg)?;
            let array_type = types::Type::Array(type_id);
            let array_type_id = self.typereg.get_or_intern(&array_type);
            t = Value::Type(array_type_id);
        }
        let type_id = t.typify(&mut self.strint, &mut self.typereg)?;
        Ok(type_id)
    }
    pub fn type_assert(&mut self, found: &TypeId, expected: &TypeId) -> anyhow::Result<()> {
        if !supports(found, expected, &self.typereg).unwrap_or(false) {
            bail!(
                "Type assertion failed: expected {:?}, found {:?}",
                self.typereg.resolve(*expected),
                self.typereg.resolve(*found)
            );
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Evaluation {
    pub value: Value,
    pub reason: EvalReason,
}
impl Evaluation {
    pub fn map<F>(self, f: F) -> Self
    where
        F: FnOnce(Value) -> Value,
    {
        Self {
            value: f(self.value),
            reason: self.reason,
        }
    }
}
pub trait DoubleMap {
    fn map<F>(self, f: F) -> Self
    where
        F: FnOnce(Value) -> Value;
}
impl DoubleMap for anyhow::Result<Evaluation, String> {
    fn map<F>(self, f: F) -> Self
    where
        F: FnOnce(Value) -> Value,
    {
        self.map(|eval| eval.map(f))
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum EvalReason {
    Return,
    Break,
    Neither,
}
impl Into<Value> for Evaluation {
    fn into(self) -> Value {
        self.value
    }
}
impl Into<Evaluation> for Value {
    fn into(self) -> Evaluation {
        Evaluation {
            value: self,
            reason: EvalReason::Neither,
        }
    }
}
