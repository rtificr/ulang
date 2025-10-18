use interns::Interner;
use pest::Parser;
use slab::Slab;
use std::{collections::HashMap, fs::File, io::{stdin, Read}, time::Instant};
use string_interner::{StringInterner, backend::BucketBackend};

use crate::{
    ast::{SpannedNode, TypeId, TypeIdent},
    noder::Noder,
    printexpr::pretty_print,
    runtime::{Runtime, types::Type},
};

mod ast;
mod err;
mod module;
mod noder;
mod printexpr;
mod runtime;
mod util;

#[derive(pest_derive::Parser)]
#[grammar = "grammar.pest"]
pub struct UParser;

fn main() {
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
    pretty_print(&nodereg, &strint, root);
    let runtime_start = Instant::now();
    let mut runtime = Runtime::new(nodereg, strint, typereg, &buf);
    runtime.init_ministd();
    runtime.init_types();

    // Run the interpreter on a separate OS thread with a bigger stack to
    // avoid stack overflow for deeply recursive programs (like the stress test).
    let handle = std::thread::Builder::new()
        .name("ulang-runtime".into())
        .stack_size(32 * 1024 * 1024)
        .spawn(move || -> anyhow::Result<(String, std::time::Duration)> {
            let eval_res = runtime.eval(root)?;
            let value_str =
                runtime.value_to_string(runtime.memory.err_get(eval_res.val_ptr).unwrap())?;
            let runtime_duration = runtime_start.elapsed();
            Ok((value_str, runtime_duration))
        })
        .expect("Failed to spawn runtime thread");

    match handle.join() {
        Ok(Ok((value_str, runtime_duration))) => {
            println!("Runtime in {:?}", runtime_duration);
            println!("Result: {}", value_str);
        }
        Ok(Err(e)) => {
            eprintln!("Runtime error: {}", e);
            return;
        }
        Err(_) => {
            eprintln!("Runtime thread panicked");
            return;
        }
    }
    stdin().read(&mut [0u8]).unwrap();

}

pub type NodeReg = Slab<SpannedNode>;
pub type TypeReg = Interner<Type>;
pub type TypeIdentMap = HashMap<TypeIdent, TypeId>;
pub type StringInt = StringInterner<BucketBackend<usize>>;
