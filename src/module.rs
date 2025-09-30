use std::{collections::HashMap, io::Read};
use anyhow::anyhow;
use pest::Parser;

use crate::{ast::{NodeId, StringId}, noder::Noder, runtime::{value::Value, Runtime}, NodeReg, Rule, StringInt, UParser};

pub struct ModuleLoader {
    pub cache: HashMap<String, Module>,
}
impl ModuleLoader {
    pub fn new() -> Self {
        Self {
            cache: HashMap::new(),
        }
    }
    pub fn load(&mut self, path: &str, strint: &mut StringInt, global_nodes: &mut NodeReg) -> anyhow::Result<&Module> {
        if !self.cache.contains_key(path) {
            let mut file = std::fs::File::open(path)?;
            let mut text = String::new();
            file.read_to_string(&mut text)?;
            let module = Module::new(&text, self, strint, global_nodes).map_err(|e| anyhow!("\nFailed to run module {}: {}", path, e))?;
            self.cache.insert(path.to_string(), module);
        }
        Ok(self.cache.get(path).unwrap())
    }
}
pub struct Module {
    pub exports: HashMap<StringId, Value>,
}
impl Module {
    pub fn new(text: &str, modules: &mut ModuleLoader, strint: &mut StringInt, global_nodes: &mut NodeReg) -> anyhow::Result<Self> {
        let pairs = UParser::parse(Rule::file, text).unwrap_or_else(|e| panic!("{}", e));
        let mut noder = Noder::new();
        noder.use_strint(strint.clone());
        let root = noder.handle_pair(pairs.into_iter().next().unwrap())?.ok_or(anyhow!("No root node found"))?;
        let (nodes, _strint, typereg) = noder.finish();
        
        // Add all nodes to the global registry
        for (_, node) in nodes {
            global_nodes.insert(node);
        }
        
        let mut runtime = Runtime::new(global_nodes, _strint, typereg, modules);
        runtime.init_types();
        runtime.init_ministd();
        runtime.eval(root)?;
        *strint = runtime.strint;
        Ok(Module {
            exports: runtime.exports,
        })
    }
}