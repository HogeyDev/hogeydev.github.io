use crate::ir::*;
use crate::regalloc::StackAllocator;

pub struct Codegen {
    output: String,
    alloc: StackAllocator,
}

impl Codegen {
    pub fn new(alloc: StackAllocator) -> Self {
        Self { output: String::new(), alloc }
    }

    fn emit(&mut self, s: &str) {
        self.output.push_str(s);
        self.output.push('\n');
    }

    pub fn generate(&mut self, module: &IrModule) -> String {
        self.emit("section .text");
        self.emit("global main");
        self.emit("");
        for func in &module.funcs {
            self.gen_func(func);
        }
        std::mem::take(&mut self.output)
    }

    fn gen_func(&mut self, func: &IrFunction) {
        self.alloc.allocate(func.num_vregs);
        self.emit(&format!("{}:", func.name));
        self.emit("  push rbp");
        self.emit("  mov rbp, rsp");
        self.emit(&format!("  sub rsp, {}", self.alloc.frame_size));

        let arg_regs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
        for (i, &pv) in func.param_vregs.iter().enumerate() {
            if i < arg_regs.len() {
                self.emit(&format!("  mov [rbp-{}], {}", self.alloc.offset(pv), arg_regs[i]));
            }
        }

        for block in &func.blocks {
            self.gen_block(block, func);
        }
        self.emit("");
    }

    fn gen_block(&mut self, block: &IrBlock, _func: &IrFunction) {
        self.emit(&format!(".{}:", block.label));

        for instr in &block.instrs {
            match instr {
                IrInstr::Const(vreg, val, _) => {
                    let off = self.alloc.offset(*vreg);
                    self.emit(&format!("  mov rax, {}", val));
                    self.emit(&format!("  mov [rbp-{}], rax", off));
                }
                IrInstr::Mov(dst, src) => {
                    let d = self.alloc.offset(*dst);
                    let s = self.alloc.offset(*src);
                    self.emit(&format!("  mov rax, [rbp-{}]", s));
                    self.emit(&format!("  mov [rbp-{}], rax", d));
                }
                IrInstr::Add(dst, a, b) => self.binop("add", dst, a, b),
                IrInstr::Sub(dst, a, b) => self.binop("sub", dst, a, b),
                IrInstr::Mul(dst, a, b) => self.binop("imul", dst, a, b),
                IrInstr::Div(dst, a, b) => {
                    let da = self.alloc.offset(*a);
                    let db = self.alloc.offset(*b);
                    let dd = self.alloc.offset(*dst);
                    self.emit(&format!("  mov rax, [rbp-{}]", da));
                    self.emit("  cdq");
                    self.emit(&format!("  mov rcx, [rbp-{}]", db));
                    self.emit("  idiv rcx");
                    self.emit(&format!("  mov [rbp-{}], rax", dd));
                }
                IrInstr::Eq(dst, a, b) => self.setcc("sete", dst, a, b),
                IrInstr::Lt(dst, a, b) => self.setcc("setl", dst, a, b),
                IrInstr::And(dst, a, b) => self.binop("and", dst, a, b),
                IrInstr::Or(dst, a, b) => self.binop("or", dst, a, b),
                IrInstr::Not(dst, src) => {
                    let ds = self.alloc.offset(*src);
                    let dd = self.alloc.offset(*dst);
                    self.emit(&format!("  mov rax, [rbp-{}]", ds));
                    self.emit("  cmp rax, 0");
                    self.emit("  sete al");
                    self.emit("  movzx rax, al");
                    self.emit(&format!("  mov [rbp-{}], rax", dd));
                }
                IrInstr::Neg(dst, src) => {
                    let ds = self.alloc.offset(*src);
                    let dd = self.alloc.offset(*dst);
                    self.emit(&format!("  mov rax, [rbp-{}]", ds));
                    self.emit("  neg rax");
                    self.emit(&format!("  mov [rbp-{}], rax", dd));
                }
                IrInstr::Call(result, name, args, _) => {
                    let arg_regs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
                    for (i, arg) in args.iter().enumerate() {
                        if i < arg_regs.len() {
                            let off = self.alloc.offset(*arg);
                            self.emit(&format!("  mov {}, [rbp-{}]", arg_regs[i], off));
                        }
                    }
                    self.emit(&format!("  call {}", name));
                    let dr = self.alloc.offset(*result);
                    self.emit(&format!("  mov [rbp-{}], rax", dr));
                }
            }
        }

        if let Some(term) = &block.terminator {
            match term {
                IrTerminator::Ret(vreg_opt) => {
                    if let Some(vreg) = vreg_opt {
                        let off = self.alloc.offset(*vreg);
                        self.emit(&format!("  mov rax, [rbp-{}]", off));
                    }
                    self.emit("  mov rsp, rbp");
                    self.emit("  pop rbp");
                    self.emit("  ret");
                }
                IrTerminator::Br(label) => {
                    self.emit(&format!("  jmp .{}", label));
                }
                IrTerminator::BrCond(vreg, tlabel, flabel) => {
                    let off = self.alloc.offset(*vreg);
                    self.emit(&format!("  mov rax, [rbp-{}]", off));
                    self.emit("  cmp rax, 0");
                    self.emit(&format!("  je .{}", flabel));
                    self.emit(&format!("  jmp .{}", tlabel));
                }
            }
        }
    }

    fn binop(&mut self, op: &str, dst: &VReg, a: &VReg, b: &VReg) {
        let da = self.alloc.offset(*a);
        let db = self.alloc.offset(*b);
        let dd = self.alloc.offset(*dst);
        self.emit(&format!("  mov rax, [rbp-{}]", da));
        self.emit(&format!("  mov rcx, [rbp-{}]", db));
        self.emit(&format!("  {} rax, rcx", op));
        self.emit(&format!("  mov [rbp-{}], rax", dd));
    }

    fn setcc(&mut self, cc: &str, dst: &VReg, a: &VReg, b: &VReg) {
        let da = self.alloc.offset(*a);
        let db = self.alloc.offset(*b);
        let dd = self.alloc.offset(*dst);
        self.emit(&format!("  mov rax, [rbp-{}]", da));
        self.emit(&format!("  mov rcx, [rbp-{}]", db));
        self.emit("  cmp rax, rcx");
        self.emit(&format!("  {} al", cc));
        self.emit("  movzx rax, al");
        self.emit(&format!("  mov [rbp-{}], rax", dd));
    }
}
