use crate::{ast::Operator, runtime::{value::Value, Runtime}};
use anyhow::*;
impl Runtime {
    pub fn operate(
        &mut self,
        lhs: &Value,
        rhs: &Value,
        op: &Operator,
    ) -> Result<Value> {
        match (lhs, rhs) {
            (Value::String(a), b) if *op == Operator::Add => Ok(Value::String(format!(
                "{}{}",
                a,
                self.value_to_string(b)?
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
                self.value_to_string(a)?,
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
                _ => bail!("Unsupported operator {:?} for numbers", op),
            },
            (Value::String(a), Value::String(b)) => match op {
                Operator::Add => Ok(Value::String(format!("{}{}", a, b))),
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
            _ => bail!(
                "Type mismatch for operation: {:?} and {:?}",
                lhs, rhs
            ),
        }
    }
    pub fn unary_operate(&self, value: &Value, op: &crate::ast::Operator) -> Result<Value> {
        match (value, op) {
            (Value::Number(n), Operator::Subtract) => Ok(Value::Number(-n)),
            (Value::Boolean(b), Operator::Not) => Ok(Value::Boolean(!b)),
            _ => bail!(
                "Unsupported unary operation {:?} on {:?}",
                op, value
            ),
        }
    }
}