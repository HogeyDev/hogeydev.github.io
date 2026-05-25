mod ast;
mod codegen;
mod diag;
mod ir;
mod ir_build;
mod ir_opt;
mod lexer;
mod parser;
mod regalloc;
mod span;
mod symbols;
mod token;
mod typeck;

use std::env;
use std::fs;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: l1s <input.l1s> [-o <output.asm>]");
        process::exit(1);
    }

    let input_file = &args[1];
    let output_file = if args.len() > 3 && args[2] == "-o" {
        args[3].clone()
    } else {
        let mut s = input_file.clone();
        if let Some(pos) = s.rfind('.') {
            s.truncate(pos);
        }
        s + ".asm"
    };

    let source = match fs::read_to_string(input_file) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error reading '{}': {}", input_file, e);
            process::exit(1);
        }
    };

    let source_file = span::SourceFile::new(source.clone());
    let mut diags = diag::Diagnostics::new();

    // Phase 1: Lexing & Parsing
    // Phase 1: Lexing & Parsing
    let mut parser = parser::Parser::new(&source, &mut diags);
    let program = parser.parse_program();

    if diags.has_errors() {
        diags.emit(&source_file);
        process::exit(1);
    }

    // Phase 2: Type checking
    let mut typeck = typeck::TypeChecker::new(&mut diags);
    typeck.check(&program);

    if diags.has_errors() {
        diags.emit(&source_file);
        process::exit(1);
    }

    // Phase 3: IR building (SSA construction)
    let mut ir_builder_module;
    {
        let mut ir_builder = ir_build::IrBuilder::new(&mut diags);
        ir_builder.build(&program);
        ir_builder_module = ir_builder.module;
    }

    if diags.has_errors() {
        diags.emit(&source_file);
        process::exit(1);
    }

    // Phase 4: IR optimization
    {
        let mut opt_ctx = ir_opt::OptContext::new();
        opt_ctx.run_all(&mut ir_builder_module);
    }

    // Phase 5: Register allocation
    let allocator = regalloc::RegAllocator::new();
    let mut allocs = std::collections::HashMap::new();
    for func in &ir_builder_module.funcs {
        let alloc = allocator.allocate(func);
        allocs.insert(func.name.clone(), alloc);
    }

    // Phase 6: Code generation
    let mut codegen = codegen::Codegen::new();
    let asm = codegen.generate(&ir_builder_module, &allocs);

    match fs::write(&output_file, &asm) {
        Ok(_) => {
            eprintln!("wrote {}", output_file);
        }
        Err(e) => {
            eprintln!("error writing '{}': {}", output_file, e);
            process::exit(1);
        }
    }
}
