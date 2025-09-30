use core::str;
use std::{collections::HashMap, fs::File, io::Read};

use anyhow::{anyhow, bail};
use pest::Parser;

use crate::{
    FRAMEBUF, NodeReg, Rule, SCREEN_HEIGHT, SCREEN_WIDTH, StringInt, TypeReg, UParser, WINDOW,
    ast::{Literal, Node, NodeId, StringId, TypeId, TypeIdent},
    runtime::{
        types::{Type, derive_type, supports},
        value::{BuiltinFn, Value},
    },
    scopes::Scopes,
};

pub mod types;
pub mod value;
pub mod memory;
pub mod helpers;

pub struct Runtime {
    pub nodes: NodeReg,
    pub strint: StringInt,
    pub typereg: TypeReg,
    pub scopes: Scopes<StringId, Value>,
    pub depth: usize,
    pub last_node_id: Option<NodeId>,
    pub source: String,
}
impl Runtime {
    pub fn new(nodes: NodeReg, strint: StringInt, typereg: TypeReg, source: &str) -> Self {
        Self {
            nodes,
            strint,
            typereg,
            source: source.to_string(),
            scopes: Scopes::new(),
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
                .insert_global(self.strint.get_or_intern(name), Value::Type(type_id));
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
            self.strint.get_or_intern("supports"),
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
            .insert_global(self.strint.get_or_intern("print"), Value::Builtin(print));

        fn typeof_(runtime: &mut Runtime, args: Vec<Value>) -> anyhow::Result<Value> {
            if args.len() != 1 {
                bail!("typeof expects 1 argument, got {}", args.len());
            }
            let value = &args[0];
            let type_id = value.get_type(&mut runtime.strint, &mut runtime.typereg);
            Ok(Value::Type(type_id))
        }
        self.scopes
            .insert_global(self.strint.get_or_intern("typeof"), Value::Builtin(typeof_ as BuiltinFn));

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
            .insert_global(self.strint.get_or_intern("sleep"), Value::Builtin(sleep as BuiltinFn));

        fn get_screen_width(runtime: &mut Runtime, args: Vec<Value>) -> anyhow::Result<Value> {
            if !args.is_empty() {
                bail!("get_screen_width expects 0 arguments, got {}", args.len());
            }
            Ok(Value::Number(SCREEN_WIDTH as f64))
        }
        self.scopes.insert_global(
            self.strint.get_or_intern("screenWidth"),
            Value::Builtin(get_screen_width as BuiltinFn),
        );
        fn get_screen_height(runtime: &mut Runtime, args: Vec<Value>) -> anyhow::Result<Value> {
            if !args.is_empty() {
                bail!("get_screen_height expects 0 arguments, got {}", args.len());
            }
            Ok(Value::Number(SCREEN_HEIGHT as f64))
        }
        self.scopes.insert_global(
            self.strint.get_or_intern("screenHeight"),
            Value::Builtin(get_screen_height as BuiltinFn),
        );
        fn set_pixel(runtime: &mut Runtime, args: Vec<Value>) -> anyhow::Result<Value> {
            let xy = &args[0];
            if !matches!(xy, Value::Tuple(elements) if elements.len() == 2 && matches!(elements[0], Value::Number(_)) && matches!(elements[1], Value::Number(_)))
            {
                bail!("set_pixel expects a tuple of two numbers as the first argument");
            }
            let Value::Tuple(elements) = xy else {
                bail!("set_pixel expects a tuple of two numbers as the first argument");
            };
            let x = if let Value::Number(n) = elements[0] {
                n as usize
            } else {
                0
            };
            let y = if let Value::Number(n) = elements[1] {
                n as usize
            } else {
                0
            };

            let color = &args[1];
            if !matches!(color, Value::Tuple(elements) if elements.len() == 3 && matches!(elements[0], Value::Number(_)) && matches!(elements[1], Value::Number(_)) && matches!(elements[2], Value::Number(_)))
            {
                bail!("set_pixel expects a tuple of three numbers as the second argument");
            }
            let Value::Tuple(elements) = color else {
                bail!("set_pixel expects a tuple of three numbers as the second argument");
            };
            let r = if let Value::Number(n) = elements[0] {
                (n * 255.0).clamp(0.0, 255.0) as u32
            } else {
                0
            };
            let g = if let Value::Number(n) = elements[1] {
                (n * 255.0).clamp(0.0, 255.0) as u32
            } else {
                0
            };
            let b = if let Value::Number(n) = elements[2] {
                (n * 255.0).clamp(0.0, 255.0) as u32
            } else {
                0
            };

            unsafe {
                if x < SCREEN_WIDTH && y < SCREEN_HEIGHT {
                    let offset = (y * SCREEN_WIDTH + x);
                    FRAMEBUF[offset] = r | g << 8 | b << 16;
                }
            }

            Ok(Value::Nil)
        }
        self.scopes.insert_global(
            self.strint.get_or_intern("setPixel"),
            Value::Builtin(set_pixel as BuiltinFn),
        );
        fn render(runtime: &mut Runtime, args: Vec<Value>) -> anyhow::Result<Value> {
            if !args.is_empty() {
                bail!("render expects 0 arguments, got {}", args.len());
            }
            unsafe {
                // Directly access the WINDOW static in the unsafe block
                if let Some(window) = {
                    let this = &raw mut WINDOW;
                    match *this {
                        Some(ref mut x) => Some(x),
                        None => None,
                    }
                } {
                    window
                        .update_with_buffer(FRAMEBUF, SCREEN_WIDTH, SCREEN_HEIGHT)
                        .unwrap();
                }
            }
            Ok(Value::Nil)
        }
        self.scopes.insert_global(
            self.strint.get_or_intern("render"),
            Value::Builtin(render as BuiltinFn),
        );
    }
    pub fn eval(&mut self, node_id: NodeId) -> anyhow::Result<Evaluation> {
        let node = self.nodes.get(node_id).cloned();
        self.last_node_id = Some(node_id);
        self.depth += 1;
        let r = match node {
            Some(node) => match &node.node {
                Node::Export(expr) => {
                    let expr_value = self.eval(*expr)?;
                    if let Node::Declaration { name, .. } = &self.nodes.get(*expr).unwrap().node {
                        let name = self.strint.resolve(*name).unwrap().to_string();
                        println!("exporting, doing nothing ");
                        Ok(expr_value)
                    } else {
                        bail!("Can only export declarations");
                    }
                }
                Node::FieldAccess {
                    object: object_id,
                    field: field_id,
                } => {
                    let object = self.eval(*object_id)?;
                    let field = self.strint.resolve(*field_id).unwrap().to_string();
                    let object = match &object.value {
                        Value::Object(map) => {
                            if let Some(value) = map.get(field_id) {
                                value.clone()
                            } else {
                                bail!("Field '{}' not found in object", field);
                            }
                        }
                        Value::Module(path_id) => Value::Nil,
                        _ => bail!("Cannot access field '{}' on non-object value", field),
                    };
                    Ok(object.into())
                }
                Node::Import(path_id) => {
                    let path = self.strint.resolve(*path_id).unwrap().to_string();

                    Ok(Value::Module(*path_id).into())
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
                        Ok(Value::Tuple(element_values).into())
                    }
                },
                Node::Identifier(name_id) => {
                    let name: &str = self.strint.resolve(*name_id).unwrap();
                    if let Some(value) = self.scopes.get(name_id) {
                        Ok(value.clone().into())
                    } else {
                        bail!("Undefined identifier: '{}'", name);
                    }
                }
                Node::TypeIdent(type_ident) => {
                    let base = self.strint.resolve(type_ident.base).unwrap();
                    let Some(mut t) = self.scopes.get(&type_ident.base).cloned() else {
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
                    name: name_id,
                    ann,
                    value,
                    export,
                } => {
                    let name = self.strint.resolve(*name_id).unwrap().to_string();
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

                    // Check if this declaration represents a type alias
                    // Only convert tuple/array values to type aliases, not functions or other values
                    let stored_value = match &val {
                        Value::Tuple(_) | Value::Array { .. } | Value::Type(_) => {
                            if let Ok(type_id) = val.typify(&mut self.strint, &mut self.typereg) {
                                Value::Type(type_id)
                            } else {
                                val.clone()
                            }
                        }
                        _ => val.clone(),
                    };

                    match export {
                        true => {
                            println!("exporting, doing nothing ");
                            self.scopes.insert(*name_id, stored_value);
                        }
                        false => {
                            self.scopes.insert(*name_id, stored_value);
                        }
                    };
                    Ok(Value::Type(val.get_type(&mut self.strint, &mut self.typereg)).into())
                }
                Node::Assign { name: name_id, value } => {
                    let name = self.strint.resolve(*name_id).unwrap().to_string();
                    let val = self.eval(*value)?;
                    let entry = self
                        .scopes
                        .get_mut(name_id)
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
                Node::FunctionCall { node, args } => {
                    let func = self.eval(*node)?.value;

                    if let Value::Builtin(builtin) = func {
                        let arg_values: anyhow::Result<Vec<Value>> = args
                            .iter()
                            .map(|&arg| self.eval(arg).map(|e| e.value))
                            .collect();
                        let arg_values = arg_values?;
                        return builtin(self, arg_values).map(|v| v.into());
                    }

                    let name = {
                        let span = self.nodes.get(*node).unwrap().span;
                        let str = self.source.get(span.start..span.end).unwrap();
                        str.trim().to_string()
                    };

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
                        let arg_ids: Vec<NodeId> = args.iter().copied().collect();
                        let mut arg_values = Vec::with_capacity(arg_ids.len());
                        for arg in arg_ids {
                            arg_values.push(self.eval(arg)?);
                        }

                        for (i, ((stringid, param_type_id), arg_value)) in
                            params.iter().zip(arg_values.iter()).enumerate()
                        {
                            let arg_type_id = arg_value
                                .value
                                .get_type(&mut self.strint, &mut self.typereg);

                            if !supports(param_type_id, &arg_type_id, &self.typereg).unwrap_or(true)
                            {
                                bail!(
                                    "Type mismatch for argument {} in call to {}: expected {:?}, found {:?}",
                                    i + 1,
                                    name,
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
                            self.scopes.insert(stringid, arg_value.into());
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
        r
    }
    pub fn resolve_type_ident(&mut self, type_ident: &TypeIdent) -> anyhow::Result<TypeId> {
        let base = self.strint.resolve(type_ident.base).unwrap();
        let Some(mut t) = self.scopes.get(&type_ident.base).cloned() else {
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
    pub fn finish(self) -> (NodeReg, StringInt, TypeReg) {
        (self.nodes, self.strint, self.typereg)
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
