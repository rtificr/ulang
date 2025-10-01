use anyhow::Result;
use anyhow::*;
use core::str;
use pest::Parser;
use std::{collections::HashMap, fs::File, io::Read, ptr};

use crate::{
    FRAMEBUF, NodeReg, Rule, SCREEN_HEIGHT, SCREEN_WIDTH, StringInt, TypeReg, UParser, WINDOW,
    ast::{Literal, Node, NodeId, StringId, TypeId, TypeIdent},
    runtime::{
        memory::{Memory, ValPtr},
        types::{Type, supports},
        value::{BuiltinFn, Value},
    },
    scopes::Scopes,
};

pub mod helpers;
pub mod memory;
pub mod operation;
pub mod transform;
pub mod types;
pub mod value;

pub struct Runtime {
    pub nodes: NodeReg,
    pub strint: StringInt,
    pub typereg: TypeReg,
    pub memory: Memory,
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
            memory: Memory::new(),
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
            self.memory
                .alloc_global(self.strint.get_or_intern(name), Value::Type(type_id));
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
        self.memory.alloc_global(
            self.strint.get_or_intern("supports"),
            Value::Builtin(supports as BuiltinFn),
        );

        fn print(runtime: &mut Runtime, args: Vec<Value>) -> anyhow::Result<Value> {
            println!(
                "{}",
                args.iter()
                    .map(|a| runtime.value_to_string(a))
                    .collect::<Result<Vec<_>>>()?
                    .join(" ")
            );
            Ok(Value::Nil)
        }
        self.memory.alloc_global(
            self.strint.get_or_intern("print"),
            Value::Builtin(print as BuiltinFn),
        );

        fn typeof_(runtime: &mut Runtime, args: Vec<Value>) -> anyhow::Result<Value> {
            if args.len() != 1 {
                bail!("typeof expects 1 argument, got {}", args.len());
            }
            let value = &args[0];
            let type_id = runtime.get_val_typeid(value.clone())?;
            Ok(Value::Type(type_id))
        }
        self.memory.alloc_global(
            self.strint.get_or_intern("typeof"),
            Value::Builtin(typeof_ as BuiltinFn),
        );

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
        self.memory.alloc_global(
            self.strint.get_or_intern("sleep"),
            Value::Builtin(sleep as BuiltinFn),
        );

        fn get_screen_width(runtime: &mut Runtime, args: Vec<Value>) -> anyhow::Result<Value> {
            if !args.is_empty() {
                bail!("get_screen_width expects 0 arguments, got {}", args.len());
            }
            Ok(Value::Number(SCREEN_WIDTH as f64))
        }
        self.memory.alloc_global(
            self.strint.get_or_intern("screenWidth"),
            Value::Builtin(get_screen_width as BuiltinFn),
        );
        fn get_screen_height(runtime: &mut Runtime, args: Vec<Value>) -> anyhow::Result<Value> {
            if !args.is_empty() {
                bail!("get_screen_height expects 0 arguments, got {}", args.len());
            }
            Ok(Value::Number(SCREEN_HEIGHT as f64))
        }
        self.memory.alloc_global(
            self.strint.get_or_intern("screenHeight"),
            Value::Builtin(get_screen_height as BuiltinFn),
        );
        fn set_pixel(runtime: &mut Runtime, args: Vec<Value>) -> anyhow::Result<Value> {
            let xy = &args[0];
            let Value::Tuple(elements) = xy else {
                bail!("set_pixel expects a tuple of two numbers as the first argument");
            };
            let elements = elements
                .iter()
                .map(|e| runtime.memory.err_get(*e))
                .collect::<Result<Vec<_>>>()?;
            let x = if let Value::Number(n) = elements[0] {
                *n as usize
            } else {
                0
            };
            let y = if let Value::Number(n) = elements[1] {
                *n as usize
            } else {
                0
            };

            let color = &args[1];
            let Value::Tuple(elements) = color else {
                bail!("set_pixel expects a tuple of three numbers as the second argument");
            };
            let elements = elements
                .iter()
                .map(|e| runtime.memory.err_get(*e))
                .collect::<Result<Vec<_>>>()?;
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
        self.memory.alloc_global(
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
        self.memory.alloc_global(
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
                    let object = match self.memory.err_get(object.val_ptr)? {
                        Value::Object(map) => {
                            if let Some(value) = map.get(field_id) {
                                value
                            } else {
                                let field = self.strint.resolve(*field_id).unwrap().to_string();
                                bail!("Field '{}' not found in object", field);
                            }
                        }
                        Value::Module(path_id) => &self.memory.malloc(Value::Nil),
                        _ => {
                            let field = self.strint.resolve(*field_id).unwrap().to_string();
                            bail!("Cannot access field '{}' on non-object value", field);
                        }
                    };
                    Ok(Evaluation::new(*object))
                }
                Node::Reference(node) => {
                    let referenced = self.eval(*node)?;
                    Ok(self
                        .memory
                        .malloc(Value::Reference(referenced.val_ptr))
                        .into())
                }
                Node::Import(path_id) => {
                    let path = self.strint.resolve(*path_id).unwrap().to_string();

                    Ok(self.memory.malloc(Value::Module(*path_id)).into())
                }
                Node::Literal(literal) => {
                    let val = match literal {
                        Literal::Number(n) => Value::Number(*n),
                        Literal::FString { parts } => {
                            // Collect node IDs first to avoid borrowing self mutably and immutably
                            let node_ids: Vec<NodeId> = parts.iter().copied().collect();
                            let mut string_parts = Vec::with_capacity(node_ids.len());
                            for nid in node_ids {
                                let eval = self.eval(nid)?;
                                let val = self.memory.err_get(eval.val_ptr)?;
                                string_parts.push(self.value_to_string(val)?);
                            }
                            let parts = string_parts.join("");
                            Value::String(parts).into()
                        }
                        Literal::String(s) => {
                            let s = self.strint.resolve(*s).unwrap();
                            Value::String(s.to_string())
                        }
                        Literal::Boolean(b) => Value::Boolean(*b),
                        Literal::Nil => Value::Nil,
                        Literal::Array { elements } => {
                            let element_values = elements
                                .iter()
                                .map(|nid| self.eval(*nid).map(|eval| eval.val_ptr))
                                .collect::<anyhow::Result<Vec<_>>>()?;
                            Value::Array {
                                of: self.derive_type(&element_values)?,
                                elements: element_values,
                            }
                        }
                        Literal::Tuple { elements } => {
                            let element_values = elements
                                .iter()
                                .map(|nid| self.eval(*nid).map(|eval| eval.val_ptr))
                                .collect::<anyhow::Result<Vec<_>>>()?;
                            Value::Tuple(element_values)
                        }
                    };
                    Ok(self.malloc_eval(val))
                }
                Node::Identifier(name_id) => {
                    let name: &str = self.strint.resolve(*name_id).unwrap();
                    if let Some(value_id) = self.memory.search(*name_id) {
                        if let Some(value) = self.memory.get(value_id) {
                            Ok(Evaluation::new(value_id))
                        } else {
                            bail!("Value not found for identifier: '{}'", name);
                        }
                    } else {
                        bail!("Undefined identifier: '{}'", name);
                    }
                }
                Node::TypeIdent(type_ident) => {
                    let base = self.strint.resolve(type_ident.base).unwrap();
                    let Some(Some(mut t)) = self
                        .memory
                        .search(type_ident.base)
                        .map(|id| self.memory.get(id).cloned())
                    else {
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
                    Ok(self.malloc_eval(t))
                }
                Node::IfExpr {
                    condition,
                    then_branch,
                    else_branch,
                } => {
                    let ptr = self.eval(*condition)?.val_ptr;
                    let cond_value = self.memory.err_get(ptr)?.is_truthy();
                    if cond_value {
                        self.eval(*then_branch)
                    } else if let Some(else_branch) = else_branch {
                        self.eval(*else_branch)
                    } else {
                        Ok(self.memory.malloc(Value::Nil).into())
                    }
                }
                Node::WhileExpr { condition, body } => {
                    let mut val = self.memory.malloc(Value::Nil);
                    let ptr = self.eval(*condition)?.val_ptr;
                    while self.memory.err_get(ptr)?.is_truthy() {
                        let eval = self.eval(*body)?;
                        match eval.reason {
                            EvalReason::Return => return Ok(eval),
                            EvalReason::Break => break,
                            EvalReason::Neither => {
                                val = eval.val_ptr;
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
                    let val_ptr = if let Some(value) = value {
                        self.eval(*value)?.val_ptr
                    } else {
                        self.memory.malloc(Value::Nil)
                    };

                    let val = self.memory.err_get(val_ptr)?.clone();

                    if let Some(type_node_id) = ann {
                        let ty = {
                            let val = self.eval(*type_node_id)?.val_ptr;
                            let val = self.memory.err_get(val)?.clone();
                            self.interpret_as_type_literal(&val)?
                        };
                        let expected_type_id = &self.typereg.get_or_intern(&Type::Type);
                        self.type_assert(&ty, expected_type_id)?;
                        if self.get_val_typeid(val.clone())? != *expected_type_id {
                            let found_type = {
                                let id = self.get_val_typeid(val)?;
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

                    println!("linking name {name} to value {:?}", val);
                    let type_id = self.get_val_typeid(val)?;
                    if *export {
                        println!("exporting, doing nothing ");
                    }
                    self.memory.attrib(*name_id, val_ptr);
                    Ok(self.malloc_eval(Value::Type(type_id)))
                }
                Node::Assign {
                    name: name_id,
                    node,
                } => {
                    let eval = self.eval(*node)?;

                    self.memory.attrib(*name_id, eval.val_ptr);
                    Ok(eval.into())
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
                    Ok(self
                        .memory
                        .malloc(Value::Function {
                            params: resolved_params,
                            body: body.clone(),
                            return_type: self.typereg.get_or_intern(&Type::Any),
                        })
                        .into())
                }
                Node::FunctionCall { node, args } => {
                    let ptr = self.eval(*node)?.val_ptr;
                    let func = self.memory.err_get(ptr)?.clone();

                    if let Value::Builtin(builtin) = func {
                        let arg_values: Vec<Value> = args
                            .iter()
                            .map(|&arg| -> anyhow::Result<Value> {
                                let eval = self.eval(arg)?;
                                Ok(self.memory.err_get(eval.val_ptr)?.clone())
                            })
                            .collect::<anyhow::Result<Vec<_>>>()?;
                        return builtin(self, arg_values).map(|v| v.to_eval(self));
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
                            let arg_type_id = self
                                .get_val_typeid(self.memory.err_get(arg_value.val_ptr)?.clone())?;
                            let type_id = self
                                .get_val_typeid(self.memory.err_get(arg_value.val_ptr)?.clone())?;

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
                        self.memory.push_scope();
                        for (i, arg_value) in arg_values.into_iter().enumerate() {
                            let name_id = params[i].0;
                            self.memory.attrib(name_id, arg_value.val_ptr);
                        }
                        let result = self.eval(body);
                        self.memory.pop_scope();
                        result
                    } else {
                        bail!("{} is not a function", name)
                    }
                }
                Node::UnaryOp { op, expr } => {
                    let expr_value = self.eval(*expr)?;
                    let result = self
                        .unary_operate(self.memory.err_get(expr_value.val_ptr)?, op)
                        .map_err(|e| anyhow!(e))?;
                    Ok(Evaluation {
                        val_ptr: self.memory.malloc(result),
                        reason: expr_value.reason,
                    })
                }
                Node::BinaryOp { left, op, right } => {
                    let left_value = self.eval(*left)?;
                    let right_value = self.eval(*right)?;
                    let val_a = self.memory.err_get(left_value.val_ptr)?.clone();
                    let val_b = self.memory.err_get(right_value.val_ptr)?.clone();
                    let result = self.operate(&val_a, &val_b, &op).map_err(|e| anyhow!(e))?;
                    Ok(Evaluation {
                        val_ptr: self.memory.malloc(result),
                        reason: EvalReason::Neither,
                    })
                }
                Node::Block { statements } => {
                    self.memory.push_scope();
                    let mut last_eval = Evaluation {
                        val_ptr: self.memory.malloc(Value::Nil),
                        reason: EvalReason::Neither,
                    };
                    for stmt in statements {
                        last_eval = self.eval(*stmt)?;
                        // If we hit a return or break, stop executing statements
                        if matches!(last_eval.reason, EvalReason::Return | EvalReason::Break) {
                            break;
                        }
                    }
                    self.memory.pop_scope();
                    Ok(last_eval)
                }
                Node::IndexAccess { base: array, index } => {
                    let array_ptr = self.eval(*array)?.val_ptr;
                    let index_ptr = self.eval(*index)?.val_ptr;
                    let array_value = self.memory.err_get(array_ptr)?;
                    let index_value = self.memory.err_get(index_ptr)?;
                    let Value::Number(n) = index_value else {
                        bail!("Index must be a number");
                    };
                    let index = *n as usize;
                    match &array_value {
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
                        Some(expr) => self.eval(*expr)?.val_ptr,
                        None => self.memory.malloc(Value::Nil),
                    };
                    Ok(Evaluation {
                        val_ptr: value,
                        reason: EvalReason::Return,
                    })
                }
                Node::Break(_) => Ok(Evaluation {
                    val_ptr: self.memory.malloc(Value::Nil),
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
        let Some(mut t) = self.memory.search_val(type_ident.base).cloned() else {
            bail!("Undefined type identifier: {}", base);
        };
        for _ in 0..type_ident.dims {
            let type_id = self.interpret_as_type_literal(&t)?;
            let array_type = types::Type::Array(type_id);
            let array_type_id = self.typereg.get_or_intern(&array_type);
            t = Value::Type(array_type_id);
        }
        let type_id = self.interpret_as_type_literal(&t)?;
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
    pub val_ptr: ValPtr,
    pub reason: EvalReason,
}
impl Evaluation {
    pub fn new(ptr: ValPtr) -> Self {
        Self {
            val_ptr: ptr,
            reason: EvalReason::Neither,
        }
    }
    pub fn is_return(&self) -> bool {
        matches!(self.reason, EvalReason::Return)
    }
    pub fn is_break(&self) -> bool {
        matches!(self.reason, EvalReason::Break)
    }
}
impl Into<ValPtr> for Evaluation {
    fn into(self) -> ValPtr {
        self.val_ptr
    }
}
impl Into<Evaluation> for ValPtr {
    fn into(self) -> Evaluation {
        Evaluation {
            val_ptr: self,
            reason: EvalReason::Neither,
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum EvalReason {
    Return,
    Break,
    Neither,
}
