use std::collections::HashMap;
use crate::ast::*;
use crate::ir::*;
use crate::typeck::ast_type_to_ir;

pub struct IrBuilder {
    pub module: IrModule,
    func_idx: usize,
    label_counter: usize,
}

impl IrBuilder {
    pub fn new() -> Self {
        Self { module: IrModule { funcs: vec![] }, func_idx: 0, label_counter: 0 }
    }

    pub fn build(&mut self, prog: &AstProgram) {
        for decl in &prog.decls {
            if let AstDecl::Func(func) = decl {
                self.build_func(func);
            }
        }
    }

    fn new_label(&mut self, prefix: &str) -> String {
        self.label_counter += 1;
        format!("{}_{}", prefix, self.label_counter)
    }

    fn func_mut(&mut self) -> &mut IrFunction {
        &mut self.module.funcs[self.func_idx]
    }

    fn emit(&mut self, instr: IrInstr) {
        let func = self.func_mut();
        func.blocks.last_mut().unwrap().instrs.push(instr);
    }

    fn set_term(&mut self, term: IrTerminator) {
        let func = self.func_mut();
        func.blocks.last_mut().unwrap().terminator = Some(term);
    }

    fn start_block(&mut self, label: String) {
        let func = self.func_mut();
        func.blocks.push(IrBlock { label, instrs: vec![], terminator: None });
    }

    fn alloc_vreg(&mut self) -> VReg {
        let func = self.func_mut();
        let vreg = func.num_vregs;
        func.num_vregs += 1;
        vreg
    }

    fn build_func(&mut self, func: &AstFuncDecl) {
        let param_types: Vec<IrType> = func.params.iter().map(|p| ast_type_to_ir(&p.type_)).collect();
        let return_type = ast_type_to_ir(&func.return_type);
        let name = func.name.clone();

        let func_idx = self.module.funcs.len();
        self.module.funcs.push(IrFunction {
            name: name.clone(),
            param_types: param_types.clone(),
            param_vregs: vec![],
            return_type: return_type.clone(),
            blocks: vec![],
            num_vregs: 0,
        });
        self.func_idx = func_idx;

        let entry_label = self.new_label("entry");
        self.start_block(entry_label);

        let mut vars: HashMap<String, VReg> = HashMap::new();
        for param in &func.params {
            let vreg = self.alloc_vreg();
            self.func_mut().param_vregs.push(vreg);
            vars.insert(param.name.clone(), vreg);
        }

        self.build_block(&func.body, &mut vars);

        let needs_term = self.func_mut().blocks.last().unwrap().terminator.is_none();
        if needs_term {
            self.set_term(IrTerminator::Ret(None));
        }
    }

    fn build_block(&mut self, block: &AstBlock, vars: &mut HashMap<String, VReg>) {
        for stmt in &block.stmts {
            self.build_stmt(stmt, vars);
        }
    }

    fn build_stmt(&mut self, stmt: &AstStmt, vars: &mut HashMap<String, VReg>) {
        match stmt {
            AstStmt::Return(expr_opt) => {
                let vreg = expr_opt.as_ref().map(|e| self.build_expr(e, vars));
                self.set_term(IrTerminator::Ret(vreg));
            }
            AstStmt::VarDecl(decl) => {
                let vreg = self.alloc_vreg();
                if let Some(init) = &decl.init {
                    let init_vreg = self.build_expr(init, vars);
                    self.emit(IrInstr::Mov(vreg, init_vreg));
                }
                vars.insert(decl.name.clone(), vreg);
            }
            AstStmt::If(ast_if) => {
                let cond_vreg = self.build_expr(&ast_if.cond, vars);
                let then_label = self.new_label("then");
                let else_label = self.new_label("else");
                let end_label = self.new_label("endif");
                self.set_term(IrTerminator::BrCond(cond_vreg, then_label.clone(), else_label.clone()));

                self.start_block(then_label);
                self.build_block(&ast_if.then_block, vars);
                if self.func_mut().blocks.last().unwrap().terminator.is_none() {
                    self.set_term(IrTerminator::Br(end_label.clone()));
                }

                self.start_block(else_label);
                if let Some(else_branch) = &ast_if.else_branch {
                    self.build_stmt(else_branch, vars);
                }
                if self.func_mut().blocks.last().unwrap().terminator.is_none() {
                    self.set_term(IrTerminator::Br(end_label.clone()));
                }

                self.start_block(end_label);
            }
            AstStmt::While(ast_while) => {
                let loop_label = self.new_label("loop");
                let body_label = self.new_label("while_body");
                let end_label = self.new_label("while_end");

                self.start_block(loop_label.clone());
                let cond_vreg = self.build_expr(&ast_while.cond, vars);
                self.set_term(IrTerminator::BrCond(cond_vreg, body_label.clone(), end_label.clone()));

                self.start_block(body_label);
                self.build_block(&ast_while.body, vars);
                if self.func_mut().blocks.last().unwrap().terminator.is_none() {
                    self.set_term(IrTerminator::Br(loop_label));
                }

                self.start_block(end_label);
            }
            AstStmt::Block(block) => {
                self.build_block(block, vars);
            }
            AstStmt::Expr(expr) => {
                self.build_expr(expr, vars);
            }
        }
    }

    fn build_expr(&mut self, expr: &AstExpr, vars: &HashMap<String, VReg>) -> VReg {
        match expr {
            AstExpr::Int(val) => {
                let vreg = self.alloc_vreg();
                self.emit(IrInstr::Const(vreg, *val, IrType::I32));
                vreg
            }
            AstExpr::Bool(val) => {
                let vreg = self.alloc_vreg();
                self.emit(IrInstr::Const(vreg, if *val { 1 } else { 0 }, IrType::Bool));
                vreg
            }
            AstExpr::Ident(name) => {
                let var_vreg = *vars.get(name).unwrap_or_else(|| {
                    panic!("undefined variable: {}", name)
                });
                let vreg = self.alloc_vreg();
                self.emit(IrInstr::Mov(vreg, var_vreg));
                vreg
            }
            AstExpr::Binary(op, lhs, rhs) => {
                let lv = self.build_expr(lhs, vars);
                let rv = self.build_expr(rhs, vars);
                let result = self.alloc_vreg();
                let instr = match op {
                    BinaryOp::Add => IrInstr::Add(result, lv, rv),
                    BinaryOp::Sub => IrInstr::Sub(result, lv, rv),
                    BinaryOp::Mul => IrInstr::Mul(result, lv, rv),
                    BinaryOp::Div => IrInstr::Div(result, lv, rv),
                    BinaryOp::Eq => IrInstr::Eq(result, lv, rv),
                    BinaryOp::Neq => {
                        let eq_vreg = self.alloc_vreg();
                        self.emit(IrInstr::Eq(eq_vreg, lv, rv));
                        self.emit(IrInstr::Not(result, eq_vreg));
                        return result;
                    }
                    BinaryOp::Lt => IrInstr::Lt(result, lv, rv),
                    BinaryOp::Gt => {
                        let lt_vreg = self.alloc_vreg();
                        self.emit(IrInstr::Lt(lt_vreg, rv, lv));
                        return lt_vreg;
                    }
                    BinaryOp::Le => {
                        let gt_vreg = self.alloc_vreg();
                        self.emit(IrInstr::Lt(gt_vreg, rv, lv));
                        self.emit(IrInstr::Not(result, gt_vreg));
                        return result;
                    }
                    BinaryOp::Ge => {
                        let lt_vreg = self.alloc_vreg();
                        self.emit(IrInstr::Lt(lt_vreg, lv, rv));
                        self.emit(IrInstr::Not(result, lt_vreg));
                        return result;
                    }
                    BinaryOp::And => IrInstr::And(result, lv, rv),
                    BinaryOp::Or => IrInstr::Or(result, lv, rv),
                };
                self.emit(instr);
                result
            }
            AstExpr::Unary(op, operand) => {
                let ov = self.build_expr(operand, vars);
                let result = self.alloc_vreg();
                match op {
                    UnaryOp::Neg => self.emit(IrInstr::Neg(result, ov)),
                    UnaryOp::Not => self.emit(IrInstr::Not(result, ov)),
                }
                result
            }
            AstExpr::Call(name, args) => {
                let arg_vregs: Vec<VReg> = args.iter().map(|a| self.build_expr(a, vars)).collect();
                let result = self.alloc_vreg();
                let ret_type = IrType::I32;
                self.emit(IrInstr::Call(result, name.clone(), arg_vregs, ret_type));
                result
            }
            AstExpr::Cast(_, expr) => {
                self.build_expr(expr, vars)
            }
            AstExpr::Assign(lhs, rhs) => {
                let rhs_vreg = self.build_expr(rhs, vars);
                if let AstExpr::Ident(name) = lhs.as_ref() {
                    let var_vreg = *vars.get(name).unwrap_or_else(|| {
                        panic!("undefined variable: {}", name)
                    });
                    self.emit(IrInstr::Mov(var_vreg, rhs_vreg));
                    var_vreg
                } else {
                    panic!("invalid assignment target");
                }
            }
        }
    }
}
