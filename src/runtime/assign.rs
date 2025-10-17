use crate::{
    ast::{NodeId, Operator},
    runtime::{Evaluation, Runtime, memory::ValPtr, value::Value},
    util::Pipeable,
};
use anyhow::*;
impl Runtime {
    pub fn assign(&mut self, target: &ValPtr, val: &ValPtr) -> Result<Evaluation> {
        let val = val.pipe(|p| self.memory.err_get(*p))?.clone();

        let mut_val = self.memory.get_mut(*target).ok_or(anyhow!(
            "Couldn't find value attributed to pointer {}",
            target
        ))?;

        match mut_val {
            Value::Reference(ptr) => {
                let ptr = ptr.clone();
                *self.memory.err_get_mut(ptr)? = val
            }
            _ => *mut_val = val,
        }

        Ok(Evaluation::new(self.malloc(Value::Nil)))
    }
    pub fn un_op(&mut self, expr: &NodeId, op: &Operator) -> Result<Evaluation> {
        let ev = self.eval(*expr)?;
        let ptr = self.collapse_eval(&ev)?;
        let val = self.memory.err_get(ptr)?.clone();
        let out = self.unary_operate(&val, op)?;
        Ok(self.malloc(out).into())
    }
    pub fn bin_op(&mut self, left: &ValPtr, op: &Operator, right: &ValPtr) -> Result<Evaluation> {
        let lval = self.memory.err_get(*left)?.clone();
        let rval = self.memory.err_get(*right)?.clone();
        let out = self.operate(&lval, &rval, op)?;
        Ok(self.malloc(out).into())
    }
}
