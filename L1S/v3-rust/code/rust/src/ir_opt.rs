use crate::ir::*;
use std::collections::{HashMap, HashSet};

pub struct OptContext {
    changed: bool,
}

impl OptContext {
    pub fn new() -> Self {
        OptContext { changed: false }
    }

    pub fn run_all(&mut self, module: &mut IrModule) -> bool {
        let mut total_changed = false;
        loop {
            self.changed = false;
            self.constant_fold(module);
            self.algebraic_simplifications(module);
            self.copy_propagation(module);
            self.dead_code_elimination(module);
            if !self.changed {
                break;
            }
            total_changed = true;
        }
        total_changed
    }

    fn constant_fold(&mut self, module: &mut IrModule) {
        let func_names: Vec<String> = module.funcs.iter().map(|f| f.name.clone()).collect();
        for func in &mut module.funcs {
            let block_addrs: Vec<String> = func.blocks.iter().map(|b| b.label.clone()).collect();
            let func_ptr = func as *const IrFunction;
            for block in &mut func.blocks {
                let mut new_instrs = Vec::new();
                for instr in &block.instrs {
                    let folded = self.try_fold(instr, block, unsafe { &*func_ptr });
                    new_instrs.push(folded);
                }
                block.instrs = new_instrs;
            }
        }
    }

    fn try_fold(&mut self, instr: &IrInstr, block: &IrBlock, _func: &IrFunction) -> IrInstr {
        match instr {
            IrInstr::Add(d, a, b) => {
                if let (Some(ca), Some(cb)) = (self.is_const(*a, block), self.is_const(*b, block))
                {
                    self.changed = true;
                    IrInstr::Const(*d, ca + cb, IrType::I32)
                } else {
                    instr.clone()
                }
            }
            IrInstr::Sub(d, a, b) => {
                if let (Some(ca), Some(cb)) = (self.is_const(*a, block), self.is_const(*b, block))
                {
                    self.changed = true;
                    IrInstr::Const(*d, ca - cb, IrType::I32)
                } else {
                    instr.clone()
                }
            }
            IrInstr::Mul(d, a, b) => {
                if let (Some(ca), Some(cb)) = (self.is_const(*a, block), self.is_const(*b, block))
                {
                    self.changed = true;
                    IrInstr::Const(*d, ca * cb, IrType::I32)
                } else {
                    instr.clone()
                }
            }
            IrInstr::Div(d, a, b) => {
                if let (Some(ca), Some(cb)) = (self.is_const(*a, block), self.is_const(*b, block))
                {
                    if cb != 0 {
                        self.changed = true;
                        IrInstr::Const(*d, ca / cb, IrType::I32)
                    } else {
                        instr.clone()
                    }
                } else {
                    instr.clone()
                }
            }
            IrInstr::Eq(d, a, b) => {
                if let (Some(ca), Some(cb)) = (self.is_const(*a, block), self.is_const(*b, block))
                {
                    self.changed = true;
                    IrInstr::Const(*d, if ca == cb { 1 } else { 0 }, IrType::Bool)
                } else {
                    instr.clone()
                }
            }
            IrInstr::Neq(d, a, b) => {
                if let (Some(ca), Some(cb)) = (self.is_const(*a, block), self.is_const(*b, block))
                {
                    self.changed = true;
                    IrInstr::Const(*d, if ca != cb { 1 } else { 0 }, IrType::Bool)
                } else {
                    instr.clone()
                }
            }
            IrInstr::Lt(d, a, b) => {
                if let (Some(ca), Some(cb)) = (self.is_const(*a, block), self.is_const(*b, block))
                {
                    self.changed = true;
                    IrInstr::Const(*d, if ca < cb { 1 } else { 0 }, IrType::Bool)
                } else {
                    instr.clone()
                }
            }
            IrInstr::Gt(d, a, b) => {
                if let (Some(ca), Some(cb)) = (self.is_const(*a, block), self.is_const(*b, block))
                {
                    self.changed = true;
                    IrInstr::Const(*d, if ca > cb { 1 } else { 0 }, IrType::Bool)
                } else {
                    instr.clone()
                }
            }
            IrInstr::Le(d, a, b) => {
                if let (Some(ca), Some(cb)) = (self.is_const(*a, block), self.is_const(*b, block))
                {
                    self.changed = true;
                    IrInstr::Const(*d, if ca <= cb { 1 } else { 0 }, IrType::Bool)
                } else {
                    instr.clone()
                }
            }
            IrInstr::Ge(d, a, b) => {
                if let (Some(ca), Some(cb)) = (self.is_const(*a, block), self.is_const(*b, block))
                {
                    self.changed = true;
                    IrInstr::Const(*d, if ca >= cb { 1 } else { 0 }, IrType::Bool)
                } else {
                    instr.clone()
                }
            }
            IrInstr::Not(d, s) => {
                if let Some(c) = self.is_const(*s, block) {
                    self.changed = true;
                    IrInstr::Const(*d, if c == 0 { 1 } else { 0 }, IrType::Bool)
                } else {
                    instr.clone()
                }
            }
            IrInstr::Neg(d, s) => {
                if let Some(c) = self.is_const(*s, block) {
                    self.changed = true;
                    IrInstr::Const(*d, -c, IrType::I32)
                } else {
                    instr.clone()
                }
            }
            _ => instr.clone(),
        }
    }

    fn is_const(&self, vreg: VReg, block: &IrBlock) -> Option<i64> {
        for instr in &block.instrs {
            if let IrInstr::Const(d, val, _) = instr {
                if *d == vreg {
                    return Some(*val);
                }
            }
        }
        None
    }

    fn algebraic_simplifications(&mut self, module: &mut IrModule) {
        for func in &mut module.funcs {
            for block in &mut func.blocks {
                let mut new_instrs = Vec::new();
                for instr in &block.instrs {
                    let simplified = self.try_simplify(instr, block);
                    new_instrs.push(simplified);
                }
                block.instrs = new_instrs;
            }
        }
    }

    fn try_simplify(&mut self, instr: &IrInstr, block: &IrBlock) -> IrInstr {
        match instr {
            IrInstr::Add(d, a, b) => {
                if let Some(c) = self.is_const(*b, block) {
                    if c == 0 {
                        self.changed = true;
                        return IrInstr::Copy(*d, *a);
                    }
                }
                if let Some(c) = self.is_const(*a, block) {
                    if c == 0 {
                        self.changed = true;
                        return IrInstr::Copy(*d, *b);
                    }
                }
                instr.clone()
            }
            IrInstr::Sub(d, a, b) => {
                if let Some(c) = self.is_const(*b, block) {
                    if c == 0 {
                        self.changed = true;
                        return IrInstr::Copy(*d, *a);
                    }
                }
                if let Some(c) = self.is_const(*a, block) {
                    if c == 0 {
                        self.changed = true;
                        return IrInstr::Neg(*d, *b);
                    }
                }
                instr.clone()
            }
            IrInstr::Mul(d, a, b) => {
                if let Some(c) = self.is_const(*b, block) {
                    if c == 1 {
                        self.changed = true;
                        return IrInstr::Copy(*d, *a);
                    }
                    if c == 0 {
                        self.changed = true;
                        return IrInstr::Const(*d, 0, IrType::I32);
                    }
                }
                if let Some(c) = self.is_const(*a, block) {
                    if c == 1 {
                        self.changed = true;
                        return IrInstr::Copy(*d, *b);
                    }
                    if c == 0 {
                        self.changed = true;
                        return IrInstr::Const(*d, 0, IrType::I32);
                    }
                }
                instr.clone()
            }
            IrInstr::Div(d, a, b) => {
                if let Some(c) = self.is_const(*b, block) {
                    if c == 1 {
                        self.changed = true;
                        return IrInstr::Copy(*d, *a);
                    }
                }
                instr.clone()
            }
            _ => instr.clone(),
        }
    }

    fn copy_propagation(&mut self, module: &mut IrModule) {
        for func in &mut module.funcs {
            let mut copies: HashMap<VReg, VReg> = HashMap::new();

            for block in &mut func.blocks {
                let mut new_instrs = Vec::new();
                for instr in &block.instrs {
                    let propagated = self.try_propagate(instr, &copies);
                    if let IrInstr::Copy(d, s) = &propagated {
                        if *d != *s {
                            copies.insert(*d, *s);
                            self.changed = true;
                        }
                    }
                    new_instrs.push(propagated);
                }
                block.instrs = new_instrs;
            }
        }
    }

    fn try_propagate(&self, instr: &IrInstr, copies: &HashMap<VReg, VReg>) -> IrInstr {
        let resolve = |v: VReg| -> VReg {
            let mut v = v;
            while let Some(&src) = copies.get(&v) {
                v = src;
            }
            v
        };

        match instr {
            IrInstr::Phi(d, ops) => {
                let new_ops: Vec<(VReg, String)> =
                    ops.iter().map(|(v, l)| (resolve(*v), l.clone())).collect();
                IrInstr::Phi(*d, new_ops)
            }
            IrInstr::Add(d, a, b) => IrInstr::Add(*d, resolve(*a), resolve(*b)),
            IrInstr::Sub(d, a, b) => IrInstr::Sub(*d, resolve(*a), resolve(*b)),
            IrInstr::Mul(d, a, b) => IrInstr::Mul(*d, resolve(*a), resolve(*b)),
            IrInstr::Div(d, a, b) => IrInstr::Div(*d, resolve(*a), resolve(*b)),
            IrInstr::Eq(d, a, b) => IrInstr::Eq(*d, resolve(*a), resolve(*b)),
            IrInstr::Neq(d, a, b) => IrInstr::Neq(*d, resolve(*a), resolve(*b)),
            IrInstr::Lt(d, a, b) => IrInstr::Lt(*d, resolve(*a), resolve(*b)),
            IrInstr::Gt(d, a, b) => IrInstr::Gt(*d, resolve(*a), resolve(*b)),
            IrInstr::Le(d, a, b) => IrInstr::Le(*d, resolve(*a), resolve(*b)),
            IrInstr::Ge(d, a, b) => IrInstr::Ge(*d, resolve(*a), resolve(*b)),
            IrInstr::Not(d, s) => IrInstr::Not(*d, resolve(*s)),
            IrInstr::Neg(d, s) => IrInstr::Neg(*d, resolve(*s)),
            IrInstr::And(d, a, b) => IrInstr::And(*d, resolve(*a), resolve(*b)),
            IrInstr::Or(d, a, b) => IrInstr::Or(*d, resolve(*a), resolve(*b)),
            IrInstr::Load(d, addr, ty) => IrInstr::Load(*d, resolve(*addr), ty.clone()),
            IrInstr::LoadStack(d, slot, ty) => IrInstr::LoadStack(*d, *slot, ty.clone()),
            IrInstr::Store(val, addr, ty) => {
                IrInstr::Store(resolve(*val), resolve(*addr), ty.clone())
            }
            IrInstr::StoreStack(val, slot, ty) => {
                IrInstr::StoreStack(resolve(*val), *slot, ty.clone())
            }
            IrInstr::Alloc(d, ty) => IrInstr::Alloc(*d, ty.clone()),
            IrInstr::Call(d, name, args, ty) => {
                let new_args: Vec<VReg> = args.iter().map(|a| resolve(*a)).collect();
                IrInstr::Call(*d, name.clone(), new_args, ty.clone())
            }
            IrInstr::Copy(d, s) => IrInstr::Copy(*d, resolve(*s)),
            IrInstr::Const(_, _, _) => instr.clone(),
        }
    }

    fn dead_code_elimination(&mut self, module: &mut IrModule) {
        for func in &mut module.funcs {
            let mut used: HashSet<VReg> = HashSet::new();
            let mut defined: HashMap<VReg, usize> = HashMap::new();
            let mut instr_indices: HashMap<usize, VReg> = HashMap::new();

            for block in &mut func.blocks {
                for (i, instr) in block.instrs.iter().enumerate() {
                    if instr.is_side_effect() {
                        if let Some(d) = instr.dest() {
                            used.insert(d);
                        }
                    }
                    for u in instr.uses() {
                        used.insert(u);
                    }
                    if let Some(d) = instr.dest() {
                        defined.insert(d, i);
                        instr_indices.insert(i, d);
                    }
                }
                match &block.terminator {
                    IrTerminator::Ret(Some(v)) => {
                        used.insert(*v);
                    }
                    IrTerminator::BrCond(v, _, _) => {
                        used.insert(*v);
                    }
                    _ => {}
                }
            }

            loop {
                let mut new_used = false;
                for block in &func.blocks {
                    for instr in &block.instrs {
                        let dest = instr.dest();
                        let essential = dest.map_or(true, |d| used.contains(&d));
                        if essential || instr.is_side_effect() {
                            for u in instr.uses() {
                                if !used.contains(&u) {
                                    used.insert(u);
                                    new_used = true;
                                }
                            }
                        }
                    }
                }
                if !new_used {
                    break;
                }
            }

            for block in &mut func.blocks {
                let mut new_instrs = Vec::new();
                for instr in &block.instrs {
                    let keep = match instr.dest() {
                        Some(d) => used.contains(&d) || instr.is_side_effect(),
                        None => instr.is_side_effect(),
                    };
                    if keep {
                        new_instrs.push(instr.clone());
                    } else {
                        self.changed = true;
                    }
                }
                block.instrs = new_instrs;
            }
        }
    }
}
