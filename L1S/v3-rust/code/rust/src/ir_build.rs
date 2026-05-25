use crate::ast::*;
use crate::diag::Diagnostics;
use crate::ir::*;
use crate::span::Span;
use crate::symbols::ast_type_to_ir;
use std::collections::HashMap;

pub struct IrBuilder<'a> {
    pub module: IrModule,
    diags: &'a mut Diagnostics,
    current_func_idx: usize,
    current_block: String,
    block_counter: usize,
    vreg_counter: usize,
    var_slot_counter: usize,
    var_slots: HashMap<String, usize>,
    blocks: HashMap<String, BlockState>,
}

#[derive(Clone)]
struct BlockState {
    preds: Vec<String>,
    sealed: bool,
    instrs: Vec<IrInstr>,
    terminator: IrTerminator,
}

impl BlockState {
    fn new() -> Self {
        BlockState {
            preds: Vec::new(),
            sealed: false,
            instrs: Vec::new(),
            terminator: IrTerminator::Unreachable,
        }
    }
}

impl<'a> IrBuilder<'a> {
    pub fn new(diags: &'a mut Diagnostics) -> Self {
        IrBuilder {
            module: IrModule::new(),
            diags,
            current_func_idx: 0,
            current_block: String::new(),
            block_counter: 0,
            vreg_counter: 0,
            var_slot_counter: 0,
            var_slots: HashMap::new(),
            blocks: HashMap::new(),
        }
    }

    pub fn build(&mut self, program: &AstProgram) {
        for decl in &program.decls {
            match decl {
                AstDecl::Func(f) => self.build_function(f),
            }
        }
    }

    fn build_function(&mut self, func: &AstFuncDecl) {
        let ret_type = ast_type_to_ir(&func.return_type);
        let ir_func = IrFunction::new(func.name.clone(), ret_type);
        self.module.funcs.push(ir_func);
        self.current_func_idx = self.module.funcs.len() - 1;
        self.vreg_counter = 0;
        self.block_counter = 0;
        self.var_slot_counter = 0;
        self.var_slots.clear();
        self.blocks.clear();

        let entry_label = self.gen_label("entry");
        self.add_block(&entry_label);
        let cb = entry_label.clone();
        self.set_current_block(&cb);

        let params: Vec<IrParam> = func
            .params
            .iter()
            .map(|param| {
                let vreg = self.module.funcs[self.current_func_idx].num_vregs;
                self.module.funcs[self.current_func_idx].num_vregs += 1;
                let ir_type = ast_type_to_ir(&param.type_);
                IrParam { name: param.name.clone(), vreg, type_: ir_type.clone() }
            })
            .collect();
        for param in &params {
            let slot = self.alloc_var_slot();
            self.emit_instr(IrInstr::StoreStack(param.vreg, slot, param.type_.clone()));
            self.write_var_slot(&param.name, slot);
        }
        self.module.funcs[self.current_func_idx].params = params;

        self.seal_block(&entry_label);

        for stmt in &func.body {
            let cb = self.current_block.clone();
            if self.block_has_terminator(&cb) {
                break;
            }
            self.process_stmt(stmt);
        }

        let last_block = self.current_block.clone();
        let has_term = self.block_has_terminator(&last_block);
        if !has_term {
            let is_unreachable = self.blocks.get(&last_block)
                .map_or(true, |s| s.preds.is_empty());
            if !is_unreachable && !matches!(func.return_type, AstType::Prim(PrimType::Void)) {
                self.diags
                    .error("function body does not return a value", Some(func.name_span));
            }
            if !is_unreachable {
                self.set_terminator(&last_block, IrTerminator::Ret(None));
            }
        }

        self.module.funcs[self.current_func_idx].num_vregs = self.vreg_counter;
        self.module.funcs[self.current_func_idx].num_var_slots = self.var_slot_counter;

        let mut ordered: Vec<IrBlock> = Vec::new();
        let mut visited = std::collections::HashSet::new();
        let mut order = Vec::new();
        self.dfs_order(&entry_label, &mut order, &mut visited);

        for label in &order {
            if let Some(state) = self.blocks.get(label) {
                let mut block = IrBlock::new(label.clone());
                block.instrs = state.instrs.clone();
                block.terminator = state.terminator.clone();
                ordered.push(block);
            }
        }

        self.module.funcs[self.current_func_idx].blocks = ordered;
    }

    fn dfs_order(
        &self,
        label: &str,
        order: &mut Vec<String>,
        visited: &mut std::collections::HashSet<String>,
    ) {
        if !visited.insert(label.to_string()) {
            return;
        }
        order.push(label.to_string());
        if let Some(state) = self.blocks.get(label) {
            match &state.terminator {
                IrTerminator::Br(t) => self.dfs_order(t, order, visited),
                IrTerminator::BrCond(_, t, f) => {
                    self.dfs_order(t, order, visited);
                    self.dfs_order(f, order, visited);
                }
                _ => {}
            }
        }
    }

    fn process_stmt(&mut self, stmt: &SpStmt) {
        let cb = self.current_block.clone();
        if self.block_has_terminator(&cb) {
            return;
        }
        match &stmt.0 {
            StmtKind::Return(expr_opt) => {
                let vreg = expr_opt.as_ref().map(|e| self.process_expr(e));
                let cb = self.current_block.clone();
                self.set_terminator(&cb, IrTerminator::Ret(vreg));
            }
            StmtKind::VarDecl {
                name,
                type_,
                init,
            } => {
                let ir_type = ast_type_to_ir(type_);
                let slot = self.alloc_var_slot();
                if let Some(init_expr) = init {
                    let rhs = self.process_expr(init_expr);
                    self.emit_instr(IrInstr::StoreStack(rhs, slot, ir_type));
                }
                self.write_var_slot(name, slot);
            }
            StmtKind::If(cond, then_body, else_body) => {
                self.process_if(cond, then_body, else_body);
            }
            StmtKind::While(cond, body) => {
                self.process_while(cond, body);
            }
            StmtKind::Block(stmts) => {
                for s in stmts {
                    self.process_stmt(s);
                }
            }
            StmtKind::Expr(expr) => {
                self.process_expr(expr);
            }
            StmtKind::Assign(name, expr) => {
                let rhs = self.process_expr(expr);
                let cb = self.current_block.clone();
                if let Some(&slot) = self.lookup_var_slot(name, &cb) {
                    self.emit_instr(IrInstr::StoreStack(rhs, slot, IrType::I32));
                } else {
                    let slot = self.alloc_var_slot();
                    self.emit_instr(IrInstr::StoreStack(rhs, slot, IrType::I32));
                    self.write_var_slot(name, slot);
                }
            }
        }
    }

    fn process_if(
        &mut self,
        cond: &SpExpr,
        then_body: &[SpStmt],
        else_body: &Option<Vec<SpStmt>>,
    ) {
        let cond_vreg = self.process_expr(cond);

        let then_label = self.gen_label("then");
        let else_label = self.gen_label("else");
        let merge_label = self.gen_label("merge");

        {
            let cb = self.current_block.clone();
            self.add_pred(&then_label, &cb);
            self.add_pred(&else_label, &cb);
            self.set_terminator(
                &cb,
                IrTerminator::BrCond(cond_vreg, then_label.clone(), else_label.clone()),
            );
        }

        self.add_block(&then_label);
        self.set_current_block(&then_label);
        self.seal_block(&then_label);
        for s in then_body {
            self.process_stmt(s);
        }
        {
            let cb = self.current_block.clone();
            if !self.block_has_terminator(&cb) {
                self.add_pred(&merge_label, &cb);
                self.set_terminator(&cb, IrTerminator::Br(merge_label.clone()));
            }
        }

        self.add_block(&else_label);
        self.set_current_block(&else_label);
        self.seal_block(&else_label);
        if let Some(eb) = else_body {
            for s in eb {
                self.process_stmt(s);
            }
        }
        {
            let cb = self.current_block.clone();
            if !self.block_has_terminator(&cb) {
                self.add_pred(&merge_label, &cb);
                self.set_terminator(&cb, IrTerminator::Br(merge_label.clone()));
            }
        }

        self.add_block(&merge_label);
        self.set_current_block(&merge_label);
        self.seal_block(&merge_label);
    }

    fn process_while(&mut self, cond: &SpExpr, body: &[SpStmt]) {
        let header_label = self.gen_label("header");
        let body_label = self.gen_label("body");
        let exit_label = self.gen_label("exit");

        {
            let cb = self.current_block.clone();
            self.add_pred(&header_label, &cb);
            self.set_terminator(&cb, IrTerminator::Br(header_label.clone()));
        }

        self.add_block(&header_label);
        self.set_current_block(&header_label);

        let cond_vreg = self.process_expr(cond);
        {
            let cb = self.current_block.clone();
            self.add_pred(&body_label, &cb);
            self.add_pred(&exit_label, &cb);
            self.set_terminator(
                &cb,
                IrTerminator::BrCond(cond_vreg, body_label.clone(), exit_label.clone()),
            );
        }

        self.add_block(&body_label);
        self.set_current_block(&body_label);
        self.seal_block(&body_label);
        for s in body {
            self.process_stmt(s);
        }
        {
            let cb = self.current_block.clone();
            if !self.block_has_terminator(&cb) {
                self.add_pred(&header_label, &cb);
                self.set_terminator(&cb, IrTerminator::Br(header_label.clone()));
            }
        }

        self.seal_block(&header_label);

        self.add_block(&exit_label);
        self.set_current_block(&exit_label);
        self.seal_block(&exit_label);
    }

    fn process_expr(&mut self, expr: &SpExpr) -> VReg {
        match &expr.0 {
            ExprKind::Int(n) => {
                let vreg = self.new_vreg();
                self.emit_instr(IrInstr::Const(vreg, *n, IrType::I32));
                vreg
            }
            ExprKind::Bool(b) => {
                let vreg = self.new_vreg();
                self.emit_instr(IrInstr::Const(vreg, if *b { 1 } else { 0 }, IrType::Bool));
                vreg
            }
            ExprKind::Ident(name) => {
                let dest = self.new_vreg();
                let slot = self.lookup_var_slot_or_err(name, expr.1);
                self.emit_instr(IrInstr::LoadStack(dest, slot, IrType::I32));
                dest
            }
            ExprKind::Binary(op, lhs, rhs) => {
                let lv = self.process_expr(lhs);
                let rv = self.process_expr(rhs);
                let dest = self.new_vreg();
                match op {
                    BinaryOp::Add => self.emit_instr(IrInstr::Add(dest, lv, rv)),
                    BinaryOp::Sub => self.emit_instr(IrInstr::Sub(dest, lv, rv)),
                    BinaryOp::Mul => self.emit_instr(IrInstr::Mul(dest, lv, rv)),
                    BinaryOp::Div => self.emit_instr(IrInstr::Div(dest, lv, rv)),
                    BinaryOp::Eq => self.emit_instr(IrInstr::Eq(dest, lv, rv)),
                    BinaryOp::Neq => self.emit_instr(IrInstr::Neq(dest, lv, rv)),
                    BinaryOp::Lt => self.emit_instr(IrInstr::Lt(dest, lv, rv)),
                    BinaryOp::Gt => self.emit_instr(IrInstr::Gt(dest, lv, rv)),
                    BinaryOp::Le => self.emit_instr(IrInstr::Le(dest, lv, rv)),
                    BinaryOp::Ge => self.emit_instr(IrInstr::Ge(dest, lv, rv)),
                    BinaryOp::And => self.emit_instr(IrInstr::And(dest, lv, rv)),
                    BinaryOp::Or => self.emit_instr(IrInstr::Or(dest, lv, rv)),
                }
                dest
            }
            ExprKind::Unary(op, expr) => {
                let v = self.process_expr(expr);
                let dest = self.new_vreg();
                match op {
                    UnaryOp::Neg => self.emit_instr(IrInstr::Neg(dest, v)),
                    UnaryOp::Not => self.emit_instr(IrInstr::Not(dest, v)),
                }
                dest
            }
            ExprKind::Call(name, args) => {
                let arg_vregs: Vec<VReg> = args.iter().map(|a| self.process_expr(a)).collect();
                let dest = self.new_vreg();
                self.emit_instr(IrInstr::Call(dest, name.clone(), arg_vregs, IrType::I32));
                dest
            }
            ExprKind::Cast(_, expr) => {
                let v = self.process_expr(expr);
                let dest = self.new_vreg();
                self.emit_instr(IrInstr::Copy(dest, v));
                dest
            }
        }
    }

    fn alloc_var_slot(&mut self) -> usize {
        let slot = self.var_slot_counter;
        self.var_slot_counter += 1;
        slot
    }

    fn write_var_slot(&mut self, name: &str, slot: usize) {
        self.var_slots.insert(name.to_string(), slot);
    }

    fn lookup_var_slot(&self, name: &str, _block: &str) -> Option<&usize> {
        self.var_slots.get(name)
    }

    fn lookup_var_slot_or_err(&mut self, name: &str, span: Span) -> usize {
        let cb = self.current_block.clone();
        if let Some(&slot) = self.lookup_var_slot(name, &cb) {
            return slot;
        }
        let slot = self.alloc_var_slot();
        self.diags
            .error(format!("undefined variable '{}'", name), Some(span));
        slot
    }

    fn gen_label(&mut self, prefix: &str) -> String {
        let label = format!("{}_{}", prefix, self.block_counter);
        self.block_counter += 1;
        label
    }

    fn new_vreg(&mut self) -> VReg {
        let vreg = self.vreg_counter;
        self.vreg_counter += 1;
        vreg
    }

    fn add_block(&mut self, label: &str) {
        if !self.blocks.contains_key(label) {
            self.blocks.insert(label.to_string(), BlockState::new());
        }
    }

    fn set_current_block(&mut self, label: &str) {
        self.current_block = label.to_string();
    }

    fn add_pred(&mut self, block: &str, pred: &str) {
        if let Some(state) = self.blocks.get_mut(block) {
            if !state.preds.contains(&pred.to_string()) {
                state.preds.push(pred.to_string());
            }
        }
    }

    fn emit_instr(&mut self, instr: IrInstr) {
        if let Some(state) = self.blocks.get_mut(&self.current_block) {
            state.instrs.push(instr);
        }
    }

    fn set_terminator(&mut self, block: &str, term: IrTerminator) {
        if let Some(state) = self.blocks.get_mut(block) {
            state.terminator = term;
        }
    }

    fn block_has_terminator(&self, block: &str) -> bool {
        self.blocks
            .get(block)
            .map_or(false, |s| !matches!(s.terminator, IrTerminator::Unreachable))
    }

    fn seal_block(&mut self, label: &str) {
        if let Some(state) = self.blocks.get_mut(label) {
            state.sealed = true;
        }
        let unsealed: Vec<String> = self
            .blocks
            .iter()
            .filter(|(_, s)| !s.sealed)
            .map(|(k, _)| k.clone())
            .collect();
        for b in unsealed {
            let all_preds_sealed = self.blocks[&b].preds.iter().all(|p| {
                self.blocks.get(p).map_or(false, |s| s.sealed)
            });
            if !self.blocks[&b].preds.is_empty() && all_preds_sealed {
                self.seal_block(&b);
            }
        }
    }

    fn emit_instr_in_block(&mut self, block: &str, instr: IrInstr) {
        if let Some(state) = self.blocks.get_mut(block) {
            state.instrs.push(instr);
        }
    }
}
