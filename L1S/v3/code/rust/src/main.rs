mod span;
mod token;
mod lexer;
mod diag;
mod ast;
mod parser;
mod ir;
mod symbols;
mod typeck;
mod ir_build;
mod regalloc;
mod codegen;

use std::env;
use std::fs;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: l1s <source.l1s> [-o output.asm]");
        process::exit(1);
    }

    let source = fs::read_to_string(&args[1]).unwrap_or_else(|e| {
        eprintln!("Error reading {}: {}", args[1], e);
        process::exit(1);
    });

    let (tokens, mut diag) = lexer::Lexer::new(&source).tokenize();

    let program = {
        let mut parser = parser::Parser::new(tokens);
        let prog = parser.parse_program();
        diag.merge(parser.diag);
        prog
    };

    if diag.has_errors() {
        diag.emit(&source);
        process::exit(1);
    }

    let mut typeck = typeck::TypeChecker::new();
    typeck.check_program(&program);
    diag.merge(typeck.diag);

    if diag.has_errors() {
        diag.emit(&source);
        process::exit(1);
    }

    let mut ir_builder = ir_build::IrBuilder::new();
    ir_builder.build(&program);

    let mut alloc = regalloc::StackAllocator::new();
    for func in &ir_builder.module.funcs {
        alloc.allocate(func.num_vregs);
    }

    let mut codegen = codegen::Codegen::new(alloc);
    let asm = codegen.generate(&ir_builder.module);

    let output_path = if args.len() > 2 && args[2] == "-o" {
        args[3].clone()
    } else {
        let input = &args[1];
        if input.ends_with(".l1s") {
            format!("{}.asm", &input[..input.len() - 4])
        } else {
            format!("{}.asm", input)
        }
    };

    fs::write(&output_path, &asm).unwrap_or_else(|e| {
        eprintln!("Error writing {}: {}", output_path, e);
        process::exit(1);
    });

    eprintln!("Wrote {}", output_path);
}
