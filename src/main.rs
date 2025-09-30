use anyhow::bail;
use interns::Interner;
use pest::{
    Parser,
    iterators::{Pair, Pairs},
};
use slab::Slab;
use std::{collections::HashMap, fs::File, io::Read, time::Instant};
use string_interner::{
    StringInterner,
    backend::{Backend, BucketBackend, BufferBackend},
};

use crate::{ast::{FuncParam, Literal, Node, NodeId, Operator, Span, SpannedNode, StringId, TypeId, TypeIdent}, err::ParseError, module::ModuleLoader, noder::Noder, printexpr::print_expr, runtime::{types::Type, Runtime}};

mod ast;
mod noder;
mod printexpr;
mod scopes;
mod runtime;
mod err;
mod module;

#[derive(pest_derive::Parser)]
#[grammar = "grammar.pest"]
pub struct UParser;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let filename = args.get(1).map(|s| s.as_str()).unwrap_or("benchmark.u");
    let mut toplevel = ModuleLoader::new();
    let start = Instant::now();
    let mut strint = StringInt::new();
    toplevel.load(filename, &mut strint).map_err(|e| {
        eprintln!("Couldn't load {}: {}", filename, e);
        e
    }).unwrap();
    let duration = start.elapsed();
    println!("Did {} in {:?}", filename, duration);
}

pub type NodeReg = Slab<SpannedNode>;
pub type TypeReg = Interner<Type>;
pub type TypeIdentMap = HashMap<TypeIdent, TypeId>;
pub type StringInt = StringInterner<BucketBackend<StringId>>;