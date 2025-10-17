use anyhow::Result;
use anyhow::*;
use core::str;

use crate::runtime::memory::Scope;
use crate::runtime::value::Table;
use crate::util::Pipeable;
use crate::{
    FRAMEBUF, NodeReg, SCREEN_HEIGHT, SCREEN_WIDTH, StringInt, TypeReg, WINDOW,
    ast::{Literal, Node, NodeId, StringId, TypeId},
    runtime::{
        memory::{Memory, ValPtr},
        types::{Type, supports},
        value::{BuiltinFn, Value},
    },
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
            let type_id: TypeId = TypeId::from_raw(self.typereg.get_or_intern(&typ));
            let name_id = StringId::from_raw(self.strint.get_or_intern(name));
            self.memory.alloc_global(name_id, Value::Type(type_id));
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
            crate::ast::StringId::from_raw(self.strint.get_or_intern("supports")),
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
            crate::ast::StringId::from_raw(self.strint.get_or_intern("print")),
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
            crate::ast::StringId::from_raw(self.strint.get_or_intern("typeof")),
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
            crate::ast::StringId::from_raw(self.strint.get_or_intern("sleep")),
            Value::Builtin(sleep as BuiltinFn),
        );

        fn get_screen_width(runtime: &mut Runtime, args: Vec<Value>) -> anyhow::Result<Value> {
            if !args.is_empty() {
                bail!("get_screen_width expects 0 arguments, got {}", args.len());
            }
            Ok(Value::Number(SCREEN_WIDTH as f64))
        }
        self.memory.alloc_global(
            crate::ast::StringId::from_raw(self.strint.get_or_intern("screenWidth")),
            Value::Builtin(get_screen_width as BuiltinFn),
        );
        fn get_screen_height(runtime: &mut Runtime, args: Vec<Value>) -> anyhow::Result<Value> {
            if !args.is_empty() {
                bail!("get_screen_height expects 0 arguments, got {}", args.len());
            }
            Ok(Value::Number(SCREEN_HEIGHT as f64))
        }
        self.memory.alloc_global(
            crate::ast::StringId::from_raw(self.strint.get_or_intern("screenHeight")),
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
            let r = if let Value::Number(n) = elements[2] {
                (n * 255.0).clamp(0.0, 255.0) as u32
            } else {
                0
            };
            let g = if let Value::Number(n) = elements[1] {
                (n * 255.0).clamp(0.0, 255.0) as u32
            } else {
                0
            };
            let b = if let Value::Number(n) = elements[0] {
                (n * 255.0).clamp(0.0, 255.0) as u32
            } else {
                0
            };

            unsafe {
                if x < SCREEN_WIDTH && y < SCREEN_HEIGHT {
                    let offset = y * SCREEN_WIDTH + x;
                    FRAMEBUF[offset] = r | g << 8 | b << 16;
                }
            }

            Ok(Value::Nil)
        }
        self.memory.alloc_global(
            crate::ast::StringId::from_raw(self.strint.get_or_intern("setPixel")),
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
            crate::ast::StringId::from_raw(self.strint.get_or_intern("render")),
            Value::Builtin(render as BuiltinFn),
        );
    }

    /// Collapse a pointer by following `Value::Reference` indirections.
    /// Returns the final `ValPtr` that points to a non-Reference value.
    /// Detects simple cycles and returns an error if a cycle is found.
    pub fn collapse_ptr(&self, start: ValPtr) -> anyhow::Result<ValPtr> {
        // Use Floyd's tortoise and hare algorithm to detect cycles without
        // allocating a HashSet on each collapse. We advance the tortoise by
        // one reference and the hare by two; if they meet there's a cycle.
        // If either pointer reaches a non-Reference value we return that
        // terminal pointer.
        let mut tortoise = start;
        // Advance hare one step initially (so hare != tortoise unless a cycle of length 1)
        let mut hare = start;

        // Helper to step a pointer one reference forward; returns Ok(Some(next))
        // if the value is a Reference, Ok(None) if it's a non-reference terminal, or
        // Err if the pointer is missing.
        let step = |ptr: ValPtr| -> anyhow::Result<Option<ValPtr>> {
            let val = self
                .memory
                .get(ptr)
                .ok_or(anyhow!("Value ID not found while collapsing pointer"))?;
            match val {
                Value::Reference(inner) => Ok(Some(*inner)),
                _ => Ok(None),
            }
        };

        loop {
            // move tortoise by one
            match step(tortoise)? {
                Some(next_t) => tortoise = next_t,
                None => return Ok(tortoise),
            }

            // move hare by one
            match step(hare)? {
                Some(next_h) => hare = next_h,
                None => return Ok(hare),
            }

            // move hare by one more (second step)
            match step(hare)? {
                Some(next_h2) => hare = next_h2,
                None => return Ok(hare),
            }

            if tortoise == hare {
                bail!(
                    "Reference cycle detected while collapsing pointer: {:?}",
                    tortoise
                );
            }
        }
    }

    /// If the evaluation is a reference, collapse it to the referenced value pointer;
    /// otherwise return the evaluation's pointer unchanged.
    pub fn collapse_eval(&self, eval: &Evaluation) -> anyhow::Result<ValPtr> {
        // collapse regardless of AccessMode to make callers agnostic
        self.collapse_ptr(eval.val_ptr)
    }

    /// Collapse a pointer and return a reference to the final value.
    /// The returned reference is tied to &self and should not be held across
    /// mutations to memory.
    pub fn collapse_value(&self, id: ValPtr) -> anyhow::Result<&Value> {
        let final_id = self.collapse_ptr(id)?;
        self.memory.err_get(final_id)
    }
    pub fn eval(&mut self, node_id: NodeId) -> anyhow::Result<Evaluation> {
        let node = self.nodes.get(node_id.raw()).cloned();
        self.last_node_id = Some(node_id);
        self.depth += 1;
        let r = match node {
            Some(node) => match &node.node {
                Node::Block { statements } => {
                    let mut ret = self.memory.malloc(Value::Nil);
                    for id in statements {
                        ret = self.eval(*id)?.val_ptr;
                    }
                    Evaluation::new(ret)
                }
                Node::Import(_) => todo!(),
                Node::Export(_) => todo!(),
                Node::Literal(literal) => match literal {
                    Literal::Number(num) => self.memory.malloc(Value::Number(*num)),
                    Literal::String(s) => self.memory.malloc(Value::String(*s)),
                    Literal::FString { parts } => {
                        let mut s = String::new();
                        for part in parts {
                            let val = self
                                .eval_collapsed(*part)?
                                .pipe(|ptr| self.memory.err_get(ptr.val_ptr))?;
                            s.push_str(&self.value_to_string(val)?);
                        }
                        let s = self.intern_str(&s);
                        self.memory.malloc(Value::String(s))
                    }
                    Literal::Boolean(b) => self.memory.malloc(Value::Boolean(*b)).into(),
                    Literal::Nil => self.memory.malloc(Value::Nil).into(),
                    Literal::Array { elements } => {
                        let mut ptrs: Vec<ValPtr> = Vec::with_capacity(elements.len());
                        for elem in elements.iter() {
                            let ev = self.eval(*elem)?;
                            ptrs.push(ev.val_ptr);
                        }
                        let ty = self.derive_type(ptrs.as_slice())?;
                        self.memory.malloc(Value::Array {
                            of: ty,
                            elements: ptrs,
                        })
                    }
                    Literal::Tuple { elements } => {
                        let mut ptrs: Vec<ValPtr> = Vec::with_capacity(elements.len());
                        for elem in elements.iter() {
                            let ev = self.eval(*elem)?;
                            ptrs.push(ev.val_ptr);
                        }
                        self.memory.malloc(Value::Tuple(ptrs))
                    }
                    Literal::Table { elements } => {
                        let mut table = Table::new();
                        for (key, value) in elements {
                            let key_ptr = self.eval(*key)?.val_ptr;
                            let key = self.memory.err_get(key_ptr)?.clone();
                            let value = self.eval(*value)?.val_ptr;
                            table.insert(key, value);
                        }
                        self.malloc(Value::Table(table))
                    }
                    Literal::Object { elements } => {
                        let mut table = Table::new();
                        for (key, value) in elements {
                            let value = self.eval(*value)?.val_ptr;
                            table.insert(*key, value);
                        }
                        self.malloc(Value::Object(table))
                    }
                }
                .into(),
                Node::Identifier(ident) => self
                    .memory
                    .search_ptr(*ident)
                    .ok_or(anyhow!(
                        "Failed to find identifier '{}'",
                        self.resolve_str(*ident).unwrap()
                    ))?
                    .into(),
                Node::IfExpr {
                    condition,
                    then_branch,
                    else_branch,
                } => {
                    if self
                        .eval(*condition)?
                        .val_ptr
                        .pipe(|i| self.memory.err_get(i))?
                        .is_truthy()
                    {
                        self.eval(*then_branch)?
                    } else {
                        if let Some(else_branch) = else_branch {
                            self.eval(*else_branch)?
                        } else {
                            Evaluation::new(self.memory.malloc(Value::Nil))
                        }
                    }
                }
                Node::WhileExpr { condition, body } => {
                    let ret = self.memory.malloc(Value::Nil);

                    while self
                        .eval(*condition)?
                        .val_ptr
                        .pipe(|i| self.memory.err_get(i))?
                        .is_truthy()
                    {
                        let eval = self.eval(*body)?;
                        if eval.did_break() {
                            break;
                        }
                    }
                    Evaluation::new(ret)
                }
                Node::ForExpr {
                    init: _init,
                    condition: _condition,
                    update: _update,
                    body: _body,
                } => todo!(),
                Node::Declaration {
                    name,
                    ann,
                    value,
                    export: _export,
                } => {
                    let eval = if let Some(value) = value {
                        self.eval(*value)?
                    } else {
                        Evaluation::new(self.memory.malloc(Value::Nil))
                    };
                    let val_ptr = eval.val_ptr;

                    if let Some(node) = ann {
                        let ty_eval = self.eval(*node)?;
                        let ty_val = self.memory.err_get(ty_eval.val_ptr)?.clone();
                        let ty = self.interpret_as_type_literal(&ty_val)?;
                        let found_type =
                            self.get_val_typeid(self.memory.err_get(val_ptr)?.clone())?;
                        self.type_assert(&found_type, &ty)?;
                    }

                    self.memory
                        .alloc_global(*name, self.memory.err_get(val_ptr)?.clone());
                    Evaluation::new(val_ptr)
                }
                Node::Assign { target, node } => {
                    let targ = self.eval(*target)?;
                    let targ = self.refify(self.collapse_ptr(targ.val_ptr)?);

                    let val = self
                        .eval(*node)?
                        .val_ptr
                        .pipe(|p| self.memory.err_get(p))?
                        .clone();

                    let mut_val = self.memory.get_mut(targ).ok_or(anyhow!(
                        "Couldn't find value attributed to pointer {}",
                        targ
                    ))?;

                    match mut_val {
                        Value::Reference(ptr) => {
                            let ptr = ptr.clone();
                            *self.memory.err_get_mut(ptr)? = val
                        }
                        _ => *mut_val = val,
                    }

                    Evaluation::new(self.malloc(Value::Nil))
                }
                Node::Function { params, body } => {
                    // Build function value with resolved parameter types. Return type defaults to Any.
                    let mut fn_params: Vec<(StringId, TypeId)> = Vec::with_capacity(params.len());
                    for p in params.iter() {
                        let ty_id = match p.type_ {
                            Some(type_node) => {
                                let ty_val = self
                                    .eval(type_node)?
                                    .val_ptr
                                    .pipe(|p| self.memory.err_get(p))?
                                    .clone();
                                self.interpret_as_type_literal(&ty_val)?
                            }
                            None => self.alloc_type(&Type::Any),
                        };
                        fn_params.push((p.name, ty_id));
                    }
                    let return_type = self.alloc_type(&Type::Any);
                    let func = Value::Function {
                        enclosed: Scope::from_collapsed(&self.memory.scope_stack),
                        params: fn_params,
                        body: *body,
                        return_type,
                    };
                    self.malloc(func).into()
                }
                Node::FunctionCall { node, args } => {
                    // Evaluate the callee and collapse any references
                    let callee_eval = self.eval(*node)?;
                    let callee_ptr = self.collapse_eval(&callee_eval)?;
                    let callee_val_clone = self.memory.err_get(callee_ptr)?.clone();

                    match callee_val_clone {
                        Value::Builtin(f) => {
                            // Evaluate args to concrete Values
                            let mut evaluated_args: Vec<Value> = Vec::with_capacity(args.len());
                            for a in args.iter() {
                                let ev = self.eval_collapsed(*a)?;
                                let v = self.memory.err_get(ev.val_ptr)?.clone();
                                evaluated_args.push(v);
                            }
                            let result = f(self, evaluated_args)?;
                            self.malloc(result).into()
                        }
                        Value::Function {
                            params,
                            body,
                            enclosed,
                            ..
                        } => {
                            if params.len() != args.len() {
                                bail!(
                                    "Function expected {} arguments, got {}",
                                    params.len(),
                                    args.len()
                                );
                            }

                            // New function-local scope
                            self.memory.push_exclusive_scope(enclosed);

                            // Bind parameters respecting declared types. If a parameter's
                            // declared type is a Reference(T), the argument must be a
                            // Value::Reference and the referenced value must match T.
                            // Otherwise we pass-by-value: allocate a copy (if arg is a
                            // Reference we dereference it first).
                            // Precompute expected inner reference types (if any) to avoid
                            // holding immutable borrows while we later need &mut self.
                            let mut expected_inner: Vec<Option<TypeId>> =
                                Vec::with_capacity(params.len());
                            for (_name, expected_ty) in params.iter() {
                                let opt = match self.resolve_type(*expected_ty) {
                                    Some(crate::runtime::types::Type::Reference(inner)) => {
                                        Some(*inner)
                                    }
                                    _ => None,
                                };
                                expected_inner.push(opt);
                            }

                            for (i, (name, expected_ty)) in params.iter().enumerate() {
                                // Evaluate the argument (not collapsed) so we can inspect for Reference
                                let arg_eval = self.eval(args[i])?;
                                let arg_val = self.memory.err_get(arg_eval.val_ptr)?.clone();

                                if let Some(inner_type) = expected_inner[i] {
                                    // expected a Reference(inner_type)
                                    if let Value::Reference(inner_ptr) = arg_val {
                                        let inner_val_clone =
                                            self.memory.err_get(inner_ptr)?.clone();
                                        let found_type = self.get_val_typeid(inner_val_clone)?;
                                        self.type_assert(&found_type, &inner_type)?;
                                        let ref_ptr =
                                            self.memory.malloc(Value::Reference(inner_ptr));
                                        self.memory.attrib(*name, ref_ptr);
                                    } else {
                                        bail!(
                                            "Parameter '{}' expects a reference type {}, but argument is not a reference",
                                            self.resolve_str(*name).unwrap_or("<param>"),
                                            self.resolve_type_str(*expected_ty)
                                        );
                                    }
                                } else {
                                    // pass-by-value: argument must be a non-reference value.
                                    // Do NOT implicitly dereference a `&T` to `T` — `&T` is a distinct
                                    // type and must be rejected here.
                                    match arg_val {
                                        Value::Reference(inner_ptr) => {
                                            // Build a readable found-type string like "&number"
                                            let inner_val_clone =
                                                self.memory.err_get(inner_ptr)?.clone();
                                            let inner_type =
                                                self.get_val_typeid(inner_val_clone)?;
                                            let found_typename =
                                                format!("&{}", self.resolve_type_str(inner_type));
                                            bail!(
                                                "Parameter '{}' expects {}, but argument is {}",
                                                self.resolve_str(*name).unwrap_or("<param>"),
                                                self.resolve_type_str(*expected_ty),
                                                found_typename
                                            );
                                        }
                                        other => {
                                            let val_ptr = self.memory.malloc(other);
                                            let found_type = self.get_val_typeid(
                                                self.memory.err_get(val_ptr)?.clone(),
                                            )?;
                                            self.type_assert(&found_type, expected_ty)?;
                                            self.memory.attrib(*name, val_ptr);
                                        }
                                    }
                                }
                            }

                            // Execute function body
                            let result = self.eval(body)?;

                            // Pop function-local scope
                            self.memory.pop_scope();

                            // If body returned early, convert to a plain Evaluation
                            if result.did_return() {
                                Evaluation::new(result.val_ptr).with_reason(EvalReason::Return)
                            } else {
                                Evaluation::new(result.val_ptr)
                            }
                        }
                        other => bail!(
                            "Attempted to call non-function value: {}",
                            self.value_to_string(&other)?
                        ),
                    }
                }
                Node::UnaryOp { op, expr } => {
                    let ev = self.eval(*expr)?;
                    let ptr = self.collapse_eval(&ev)?;
                    let val = self.memory.err_get(ptr)?.clone();
                    let out = self.unary_operate(&val, op)?;
                    self.malloc(out).into()
                }
                Node::BinaryOp { left, op, right } => {
                    let l = self.eval(*left)?;
                    let lptr = self.collapse_eval(&l)?;
                    let lval = self.memory.err_get(lptr)?.clone();
                    let r = self.eval(*right)?;
                    let rptr = self.collapse_eval(&r)?;
                    let rval = self.memory.err_get(rptr)?.clone();
                    let out = self.operate(&lval, &rval, op)?;
                    self.malloc(out).into()
                }
                Node::IndexAccess { base, index } => {
                    let b = self.eval(*base)?;
                    let bptr = self.collapse_eval(&b)?;
                    let base_val_clone = self.memory.err_get(bptr)?.clone();
                    let i = self.eval(*index)?;
                    let iptr = self.collapse_eval(&i)?;
                    let idx_val_clone = self.memory.err_get(iptr)?.clone();

                    match base_val_clone {
                        Value::Array { elements, .. } => {
                            let idx = if let Value::Number(n) = idx_val_clone {
                                n as usize
                            } else {
                                bail!("Array index must be a number")
                            };
                            let elem_ptr =
                                *elements.get(idx).ok_or(anyhow!("Index out of bounds"))?;
                            Evaluation::new(self.refify(elem_ptr))
                        }
                        Value::Tuple(elements) => {
                            let idx = if let Value::Number(n) = idx_val_clone {
                                n as usize
                            } else {
                                bail!("Tuple index must be a number")
                            };
                            let elem_ptr =
                                *elements.get(idx).ok_or(anyhow!("Index out of bounds"))?;
                            Evaluation::new(self.refify(elem_ptr))
                        }
                        Value::Table(table) => {
                            let ptr = *table
                                .get(&idx_val_clone)
                                .unwrap_or(&self.memory.malloc(Value::Nil));
                            Evaluation::new(self.refify(ptr))
                        }
                        _ => bail!(
                            "Indexing is not supported for {}",
                            self.get_val_typeid(base_val_clone)?
                                .pipe(|t| self.resolve_type_str(t))
                        ),
                    }
                }
                Node::FieldAccess { object, field } => {
                    let eval = self.eval(*object)?;
                    let ptr = self.collapse_eval(&eval)?;
                    let val = self.memory.err_get(ptr)?.clone();

                    match val {
                        Value::Object(object) => {
                            let ptr = object.get(field).ok_or(anyhow!(
                                "Object has no key '{}'",
                                self.resolve_str(*field).unwrap_or("unknown")
                            ))?;

                            Evaluation::new(self.refify(*ptr))
                        }
                        _ => {
                            let type_id = self.get_val_typeid(val)?;
                            let typename = self.resolve_type_str(type_id);
                            bail!("{typename} does not support field access",)
                        }
                    }
                }
                Node::Reference(node) => {
                    // Create a reference to the evaluated expression
                    let ev = self.eval(*node)?;
                    let ptr = ev.val_ptr;
                    self.malloc(Value::Reference(ptr)).into()
                }
                Node::Return(expr_opt) => {
                    if let Some(expr) = expr_opt {
                        let ev = self.eval_collapsed(*expr)?;
                        Evaluation::new(ev.val_ptr).with_reason(EvalReason::Return)
                    } else {
                        Evaluation::new(self.memory.malloc(Value::Nil))
                            .with_reason(EvalReason::Return)
                    }
                }
                Node::Break(expr_opt) => {
                    if let Some(expr) = expr_opt {
                        let ev = self.eval_collapsed(*expr)?;
                        Evaluation::new(ev.val_ptr).with_reason(EvalReason::Break)
                    } else {
                        Evaluation::new(self.memory.malloc(Value::Nil))
                            .with_reason(EvalReason::Break)
                    }
                }
            },
            None => bail!("Node ID {:?} not found", node_id),
        };
        self.depth -= 1;
        Ok(r)
    }

    /// Evaluate `node_id` and return an `Evaluation` whose pointer has been
    /// collapsed through any `Value::Reference` indirections. This lets callers
    /// ignore whether the evaluated result was a reference or a value.
    pub fn eval_collapsed(&mut self, node_id: NodeId) -> anyhow::Result<Evaluation> {
        let eval = self.eval(node_id)?;
        // collapse_eval borrows &self, but we currently have &mut self; to call
        // collapse_eval we reborrow as immutable using a short-lived scope.
        let collapsed_ptr = {
            // Create a temporary immutable borrow to collapse the pointer.
            let r: &Runtime = &*self;
            r.collapse_eval(&eval)?
        };
        Ok(Evaluation {
            val_ptr: collapsed_ptr,
            reason: eval.reason,
        })
    }
    pub fn type_assert(&mut self, found: &TypeId, expected: &TypeId) -> anyhow::Result<()> {
        if !supports(expected, found, &self.typereg).unwrap_or(false) {
            bail!(
                "Type assertion failed: expected {:?}, found {:?}",
                self.typereg.resolve(expected.raw()),
                self.typereg.resolve(found.raw())
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
    pub fn did_return(&self) -> bool {
        matches!(self.reason, EvalReason::Return)
    }
    pub fn did_break(&self) -> bool {
        matches!(self.reason, EvalReason::Break)
    }
    pub fn with_reason(self, reason: EvalReason) -> Self {
        Self { reason, ..self }
    }
}
// impl Evaluation {
//     /// Collapse this evaluation's pointer by following Reference chains and
//     /// return the final `ValPtr`.
//     pub fn collapse(&self, runtime: &Runtime) -> anyhow::Result<ValPtr> {
//         runtime.collapse_eval(self)
//     }

//     /// Collapse this evaluation and return a reference to the final `Value`.
//     /// The returned reference is tied to `&runtime` and must not be held across
//     /// mutable operations on the runtime memory.
//     pub fn collapse_value<'a>(&self, runtime: &'a Runtime) -> anyhow::Result<&'a Value> {
//         self.collapse(runtime)?.pipe(|id| runtime.collapse_value(id))
//     }
// }
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
#[derive(Debug, Clone, PartialEq)]
pub enum AccessMode {
    Value,
    Reference,
}
