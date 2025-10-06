use anyhow::bail;
use interns::Interner;
use pest::{
    Parser,
    iterators::{Pair, Pairs},
};
use slab::Slab;
use std::{collections::HashMap, fs::File, io::{stdin, Read}, time::Instant};
use string_interner::{
    StringInterner,
    backend::{Backend, BucketBackend, BufferBackend},
};

use crate::{ast::{FuncParam, Literal, Node, NodeId, Operator, Span, SpannedNode, StringId, TypeId, TypeIdent, StringIdRaw, TypeIdRaw, NodeIdRaw}, err::ParseError, noder::Noder, printexpr::print_expr, runtime::{types::Type, Runtime}};

mod ast;
mod noder;
mod printexpr;
mod scopes;
mod runtime;
mod err;
mod module;
mod util;

#[derive(pest_derive::Parser)]
#[grammar = "grammar.pest"]
pub struct UParser;

const SCREEN_WIDTH: usize = 500;
const SCREEN_HEIGHT: usize = 500;
static mut FRAMEBUF: &'static mut [u32] = &mut [0; SCREEN_WIDTH * SCREEN_HEIGHT];
static mut WINDOW: Option<minifb::Window> = None;
fn main() {
    unsafe {
        WINDOW = Some(minifb::Window::new(
            "Ulang Graphics",
            SCREEN_WIDTH,
            SCREEN_HEIGHT,
            minifb::WindowOptions::default(),
        ).unwrap());
        {
            let this = &raw mut WINDOW;
            match *this {
                Some(ref mut x) => Some(x),
                None => None,
            }
        }.unwrap().update_with_buffer(FRAMEBUF, SCREEN_WIDTH, SCREEN_HEIGHT).unwrap();
    }
    let args: Vec<String> = std::env::args().collect();
    let filename = args.get(1).map(|s| s.as_str()).unwrap_or("benchmark.u");
    
    let mut file = File::open(filename).expect("Could not open file");
    let mut buf = String::new();
    file.read_to_string(&mut buf).expect("Could not read file");

    let parse_start = Instant::now();
    let pairs = UParser::parse(Rule::file, &buf).map_err(|e| format!("Parse error: {}", e));
    let parse_duration = parse_start.elapsed();
    let pairs = match pairs {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Parse error: {}", e);
            return;
        }
    };
    println!("Parsed in {:?}", parse_duration);

    let noder_start = Instant::now();
    let mut noder = Noder::new();
    let root = match noder.handle_pair(pairs.into_iter().next().unwrap()) {
        Ok(nr) => match nr {
            Some(nr) => nr,
            None => {
                eprintln!("No nodes generated");
                return;
            }
        },
        Err(e) => {
            eprintln!("Noder error: {}", e);
            return;
        }
    };
    let noder_duration = noder_start.elapsed();
    println!("Noded in {:?}", noder_duration);
    let (nodereg, strint, typereg) = noder.finish();
    print_expr(&nodereg, &strint, 0, root);
    let runtime_start = Instant::now();
    let mut runtime = Runtime::new(nodereg, strint, typereg, &buf);
    runtime.init_ministd();
    runtime.init_types();
    match runtime.eval(root) {
        Ok(evaluation) => {
            let value_str = runtime.value_to_string(runtime.memory.err_get(evaluation.val_ptr).unwrap()).unwrap();
            let (nodereg, strint, typereg) = runtime.finish();
            let runtime_duration = runtime_start.elapsed();
            println!("Runtime in {:?}", runtime_duration);
            println!("Result: {}", value_str);
        }
        Err(e) => {
            eprintln!("Runtime error: {}", e);
            return;
        }
    }

    // stdin().read(&mut [0u8]).unwrap();
}

pub type NodeReg = Slab<SpannedNode>;
pub type TypeReg = Interner<Type>;
pub type TypeIdentMap = HashMap<TypeIdent, TypeId>;
pub type StringInt = StringInterner<BucketBackend<usize>>;