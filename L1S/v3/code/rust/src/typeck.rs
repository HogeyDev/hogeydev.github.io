use crate::span::Span;
use crate::diag::Diagnostics;
use crate::ast::*;
use crate::symbols::SymbolTable;
use crate::ir::IrType;

pub struct TypeChecker {
    pub syms: SymbolTable,
    pub diag: Diagnostics,
}

pub fn ast_type_to_ir(t: &AstType) -> IrType {
    match t {
        AstType::Prim(p) => match p {
            PrimType::I32 => IrType::I32,
            PrimType::I64 => IrType::I64,
            PrimType::U32 => IrType::U32,
            PrimType::U64 => IrType::U64,
            PrimType::I8 => IrType::I8,
            PrimType::U8 => IrType::U8,
            PrimType::Bool => IrType::Bool,
            PrimType::Void => IrType::Void,
            PrimType::Char => IrType::I8,
            PrimType::Usize => IrType::U64,
            PrimType::Isize => IrType::I64,
        },
        AstType::Named(_) => IrType::I32,
        AstType::Ptr(_inner) => IrType::Ptr(0),
        AstType::Array(_, _inner) => IrType::Ptr(0),
    }
}

impl TypeChecker {
    pub fn new() -> Self {
        Self { syms: SymbolTable::new(), diag: Diagnostics::new() }
    }

    pub fn check_program(&mut self, prog: &AstProgram) {
        for decl in &prog.decls {
            if let AstDecl::Func(func) = decl {
                let param_types: Vec<IrType> = func.params.iter().map(|p| ast_type_to_ir(&p.type_)).collect();
                let ret_type = ast_type_to_ir(&func.return_type);
                self.syms.insert_func(&func.name, param_types, ret_type);
            }
        }
        for decl in &prog.decls {
            if let AstDecl::Func(func) = decl {
                self.check_func(func);
            }
        }
    }

    fn check_func(&mut self, func: &AstFuncDecl) {
        self.syms.enter_scope();
        let ret_type = ast_type_to_ir(&func.return_type);
        for param in &func.params {
            let pt = ast_type_to_ir(&param.type_);
            self.syms.insert_var(&param.name, pt);
        }
        self.check_block(&func.body, &ret_type);
        self.syms.exit_scope();
    }

    fn check_block(&mut self, block: &AstBlock, ret: &IrType) {
        for stmt in &block.stmts {
            self.check_stmt(stmt, ret);
        }
    }

    fn check_stmt(&mut self, stmt: &AstStmt, ret: &IrType) {
        match stmt {
            AstStmt::Return(expr_opt) => {
                if let Some(expr) = expr_opt {
                    let t = self.check_expr(expr);
                    if let Some(t) = t {
                        if t != *ret {
                            self.diag.error(Span::new(0, 0), format!("expected return type {:?}, got {:?}", ret, t));
                        }
                    }
                } else if *ret != IrType::Void {
                    self.diag.error(Span::new(0, 0), "expected return value");
                }
            }
            AstStmt::VarDecl(decl) => {
                let dt = ast_type_to_ir(&decl.type_);
                if let Some(init) = &decl.init {
                    let it = self.check_expr(init);
                    if let Some(it) = it {
                        if it != dt {
                            self.diag.error(Span::new(0, 0), format!("type mismatch in var decl: {:?} vs {:?}", dt, it));
                        }
                    }
                }
                self.syms.insert_var(&decl.name, dt);
            }
            AstStmt::If(ast_if) => {
                let ct = self.check_expr(&ast_if.cond);
                if let Some(IrType::Bool) = ct {} else {
                    self.diag.error(Span::new(0, 0), "if condition must be bool");
                }
                self.check_block(&ast_if.then_block, ret);
                if let Some(else_branch) = &ast_if.else_branch {
                    self.check_stmt(else_branch, ret);
                }
            }
            AstStmt::While(ast_while) => {
                let ct = self.check_expr(&ast_while.cond);
                if let Some(IrType::Bool) = ct {} else {
                    self.diag.error(Span::new(0, 0), "while condition must be bool");
                }
                self.check_block(&ast_while.body, ret);
            }
            AstStmt::Block(block) => {
                self.syms.enter_scope();
                self.check_block(block, ret);
                self.syms.exit_scope();
            }
            AstStmt::Expr(expr) => { self.check_expr(expr); }
        }
    }

    fn check_expr(&mut self, expr: &AstExpr) -> Option<IrType> {
        match expr {
            AstExpr::Int(_) => Some(IrType::I32),
            AstExpr::Bool(_) => Some(IrType::Bool),
            AstExpr::Ident(name) => {
                self.syms.lookup(name).map(|s| s.type_.clone())
                    .or_else(|| {
                        self.diag.error(Span::new(0, 0), format!("undefined variable: {}", name));
                        None
                    })
            }
            AstExpr::Binary(op, lhs, rhs) => {
                let lt = self.check_expr(lhs)?;
                let rt = self.check_expr(rhs)?;
                if lt != rt {
                    self.diag.error(Span::new(0, 0), "type mismatch in binary expression");
                }
                match op {
                    BinaryOp::And | BinaryOp::Or => {
                        if lt != IrType::Bool {
                            self.diag.error(Span::new(0, 0), "logical ops require bool");
                        }
                        Some(IrType::Bool)
                    }
                    BinaryOp::Eq | BinaryOp::Neq | BinaryOp::Lt | BinaryOp::Gt | BinaryOp::Le | BinaryOp::Ge => {
                        Some(IrType::Bool)
                    }
                    _ => Some(lt),
                }
            }
            AstExpr::Unary(op, operand) => {
                let t = self.check_expr(operand)?;
                match op {
                    UnaryOp::Neg => {
                        if t == IrType::Bool { self.diag.error(Span::new(0, 0), "cannot negate bool"); }
                        Some(t)
                    }
                    UnaryOp::Not => {
                        if t != IrType::Bool { self.diag.error(Span::new(0, 0), "not requires bool"); }
                        Some(IrType::Bool)
                    }
                }
            }
            AstExpr::Call(name, args) => {
                let (param_types_str, ret_type) = {
                    let sym = self.syms.lookup(name);
                    match sym {
                        Some(s) => match &s.kind {
                            crate::symbols::SymbolKind::Func(p, r) => (p.clone(), r.clone()),
                            _ => { self.diag.error(Span::new(0, 0), format!("not a function: {}", name)); return None; }
                        },
                        None => { self.diag.error(Span::new(0, 0), format!("undefined function: {}", name)); return None; }
                    }
                };
                if args.len() != param_types_str.len() {
                    self.diag.error(Span::new(0, 0), "wrong number of arguments");
                    return None;
                }
                for (arg, pt) in args.iter().zip(param_types_str.iter()) {
                    let at = self.check_expr(arg);
                    if let Some(at) = at {
                        if &at != pt {
                            self.diag.error(Span::new(0, 0), "argument type mismatch");
                        }
                    }
                }
                Some(ret_type)
            }
            AstExpr::Cast(type_, expr) => {
                self.check_expr(expr);
                Some(ast_type_to_ir(type_))
            }
            AstExpr::Assign(lhs, rhs) => {
                let rt = self.check_expr(rhs)?;
                if let AstExpr::Ident(name) = lhs.as_ref() {
                    let sym = self.syms.lookup(name);
                    match sym {
                        Some(s) => {
                            if s.type_ != rt {
                                self.diag.error(Span::new(0, 0), "type mismatch in assignment");
                            }
                            Some(s.type_.clone())
                        }
                        None => { self.diag.error(Span::new(0, 0), format!("undefined variable: {}", name)); None }
                    }
                } else {
                    self.diag.error(Span::new(0, 0), "invalid assignment target");
                    None
                }
            }
        }
    }
}
