use crate::{
    ast::Operator,
    runtime::{types::supports, value::Value, Runtime},
};
use anyhow::*;
impl Runtime {
    pub fn operate(&mut self, lhs: &Value, rhs: &Value, op: &Operator) -> Result<Value> {
        match (lhs, rhs) {
            (Value::String(a), b) if *op == Operator::Add => {
                Ok(Value::String(self.intern_str(&format!(
                    "{}{}",
                    self.resolve_str(*a).unwrap(),
                    self.value_to_string(b)?
                ))))
            }
            (Value::String(a), b)
                if *op == Operator::Multiply
                    && matches!(b, Value::Number(n) if *n >= 0.0 && n.fract() == 0.0) =>
            {
                let n = if let Value::Number(n) = b {
                    *n as usize
                } else {
                    0
                };
                let a = self.resolve_str(*a)
                    .unwrap()
                    .repeat(n);

                Ok(Value::String(self.intern_str(&a)))
            }
            (a, Value::String(b)) if *op == Operator::Add => {
                Ok(Value::String(self.intern_str(&format!(
                    "{}{}",
                    self.value_to_string(a)?,
                    self.resolve_str(*b).unwrap(),
                ))))
            }
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
                _ => bail!("Unsupported operator {:?} for numbers", op),
            },
            (Value::String(a), Value::String(b)) => match op {
                Operator::Add => {
                    let (a, b) = (
                        self.resolve_str(*a).unwrap(),
                        self.resolve_str(*b).unwrap(),
                    );
                    let merged = self.intern_str(&format!("{}{}", a, b));
                    Ok(Value::String(merged))
                }
                Operator::Equal => Ok(Value::Boolean(a == b)),
                Operator::NotEqual => Ok(Value::Boolean(a != b)),
                _ => bail!("Unsupported operator {:?} for strings", op),
            },
            (Value::Boolean(a), Value::Boolean(b)) => match op {
                Operator::And => Ok(Value::Boolean(*a && *b)),
                Operator::Or => Ok(Value::Boolean(*a || *b)),
                Operator::Equal => Ok(Value::Boolean(a == b)),
                Operator::NotEqual => Ok(Value::Boolean(a != b)),
                _ => bail!("Unsupported operator {:?} for booleans", op),
            },
            (Value::Type(a), Value::Type(b)) => match op {
                Operator::Equal => Ok(Value::Boolean(a == b)),
                Operator::NotEqual => Ok(Value::Boolean(a != b)),
                Operator::BitOr => Ok(Value::Type(self.merge_types(&a, &b))),
                _ => bail!("Unsupported operator {:?} for types", op),
            },
            (Value::Array { of, elements }, b) if *op == Operator::Add => {
                let b_ty = self.get_val_typeid(b.clone())?;
                if !supports(of, &b_ty, &self.typereg).ok_or(anyhow!("Missing types"))? {
                    bail!("Cannot add array of type {:?} with value of type {:?}", of, b_ty);
                }
                let mut new_elements = elements.clone();
                new_elements.push(self.malloc(b.clone()));
                Ok(Value::Array {
                    of: *of,
                    elements: new_elements,
                })
            }
            (Value::Array { of, elements }, Value::Array { of: of2, elements: elements2 }) => match op {
                Operator::Add => {
                    if of != of2 {
                        bail!("Cannot add arrays of different types");
                    }
                    let mut new_elements = elements.clone();
                    new_elements.extend(elements2.iter());
                    Ok(Value::Array {
                        of: *of,
                        elements: new_elements,
                    })
                }
                Operator::Equal => {
                    if of != of2 || elements.len() != elements2.len() {
                        return Ok(Value::Boolean(false));
                    }
                    for (e1, e2) in elements.iter().zip(elements2.iter()) {
                        let v1 = self.memory.err_get(*e1)?.clone();
                        let v2 = self.memory.err_get(*e2)?.clone();
                        if self.operate(&v1, &v2, &Operator::NotEqual)? == Value::Boolean(true) {
                            return Ok(Value::Boolean(false));
                        }
                    }
                    Ok(Value::Boolean(true))
                }
                Operator::NotEqual => {
                    if of != of2 || elements.len() != elements2.len() {
                        return Ok(Value::Boolean(true));
                    }
                    for (e1, e2) in elements.iter().zip(elements2.iter()) {
                        let v1 = self.memory.err_get(*e1)?.clone();
                        let v2 = self.memory.err_get(*e2)?.clone();
                        if self.operate(&v1, &v2, &Operator::NotEqual)? == Value::Boolean(true) {
                            return Ok(Value::Boolean(true));
                        }
                    }
                    Ok(Value::Boolean(false))
                }
                _ => bail!("Unsupported operator {:?} for arrays", op),
            },
            _ => bail!("Type mismatch for operation: {} and {}", self.value_to_string(lhs)?, self.value_to_string(rhs)?),
        }
    }
    pub fn unary_operate(&self, value: &Value, op: &crate::ast::Operator) -> Result<Value> {
        match (value, op) {
            (Value::Number(n), Operator::Subtract) => Ok(Value::Number(-n)),
            (Value::Boolean(b), Operator::Not) => Ok(Value::Boolean(!b)),
            _ => bail!("Unsupported unary operation {:?} on {:?}", op, value),
        }
    }
}
