use crate::ast::*;
use crate::diag::Diagnostics;
use crate::ir::IrType;
use crate::span::Span;
use crate::symbols::{ast_type_to_ir, SymType, SymbolTable};

pub struct TypeChecker<'a> {
    pub syms: SymbolTable,
    diags: &'a mut Diagnostics,
}

impl<'a> TypeChecker<'a> {
    pub fn new(diags: &'a mut Diagnostics) -> Self {
        TypeChecker {
            syms: SymbolTable::new(),
            diags,
        }
    }

    pub fn check(&mut self, program: &AstProgram) {
        self.collect_decls(program);
        if self.diags.has_errors() {
            return;
        }
        self.check_func_bodies(program);
    }

    fn collect_decls(&mut self, program: &AstProgram) {
        for decl in &program.decls {
            match decl {
                AstDecl::Func(f) => {
                    let params: Vec<IrType> = f
                        .params
                        .iter()
                        .map(|p| ast_type_to_ir(&p.type_))
                        .collect();
                    let ret = ast_type_to_ir(&f.return_type);
                    let sym = SymType::Func {
                        params,
                        return_type: ret,
                    };
                    if !self.syms.insert(f.name.clone(), sym) {
                        self.diags
                            .error(format!("duplicate function '{}'", f.name), Some(f.name_span));
                    }
                }
            }
        }
    }

    fn check_func_bodies(&mut self, program: &AstProgram) {
        for decl in &program.decls {
            match decl {
                AstDecl::Func(f) => {
                    self.syms.enter_scope();
                    for param in &f.params {
                        let ir_type = ast_type_to_ir(&param.type_);
                        self.syms.insert(
                            param.name.clone(),
                            SymType::Var(ir_type),
                        );
                    }
                    let ret_type = ast_type_to_ir(&f.return_type);
                    for stmt in &f.body {
                        self.check_stmt(stmt, &ret_type);
                    }
                    self.syms.exit_scope();
                }
            }
        }
    }

    fn check_stmt(&mut self, stmt: &SpStmt, expected_ret: &IrType) -> IrType {
        let span = stmt.1;
        match &stmt.0 {
            StmtKind::Return(expr_opt) => {
                if let Some(expr) = expr_opt {
                    let expr_type = self.check_expr(expr);
                    if !types_compatible(&expr_type, expected_ret) {
                        self.diags.error(
                            format!(
                                "expected return type {}, found {}",
                                expected_ret, expr_type
                            ),
                            Some(expr.1),
                        );
                    }
                } else if *expected_ret != IrType::Void {
                    self.diags.error(
                        format!("expected return value of type {}", expected_ret),
                        Some(span),
                    );
                }
                IrType::Void
            }
            StmtKind::VarDecl {
                name,
                type_,
                init,
            } => {
                let decl_type = ast_type_to_ir(type_);
                if let Some(init_expr) = init {
                    let init_type = self.check_expr(init_expr);
                    if !types_compatible(&init_type, &decl_type) {
                        self.diags.error(
                            format!(
                                "expected type {}, found {} in variable '{}'",
                                decl_type, init_type, name
                            ),
                            Some(init_expr.1),
                        );
                    }
                }
                self.syms.insert(name.clone(), SymType::Var(decl_type));
                IrType::Void
            }
            StmtKind::If(cond, then_body, else_body) => {
                let cond_type = self.check_expr(cond);
                if cond_type != IrType::Bool {
                    self.diags.error(
                        format!("if condition must be bool, found {}", cond_type),
                        Some(cond.1),
                    );
                }
                self.syms.enter_scope();
                for s in then_body {
                    self.check_stmt(s, expected_ret);
                }
                self.syms.exit_scope();
                if let Some(eb) = else_body {
                    self.syms.enter_scope();
                    for s in eb {
                        self.check_stmt(s, expected_ret);
                    }
                    self.syms.exit_scope();
                }
                IrType::Void
            }
            StmtKind::While(cond, body) => {
                let cond_type = self.check_expr(cond);
                if cond_type != IrType::Bool {
                    self.diags.error(
                        format!("while condition must be bool, found {}", cond_type),
                        Some(cond.1),
                    );
                }
                self.syms.enter_scope();
                for s in body {
                    self.check_stmt(s, expected_ret);
                }
                self.syms.exit_scope();
                IrType::Void
            }
            StmtKind::Block(stmts) => {
                self.syms.enter_scope();
                for s in stmts {
                    self.check_stmt(s, expected_ret);
                }
                self.syms.exit_scope();
                IrType::Void
            }
            StmtKind::Expr(expr) => {
                self.check_expr(expr);
                IrType::Void
            }
            StmtKind::Assign(name, expr) => {
                let var_type = match self.syms.lookup(name) {
                    Some(SymType::Var(t)) => t.clone(),
                    Some(SymType::Func { .. }) => {
                        self.diags
                            .error(format!("'{}' is a function, not a variable", name), Some(span));
                        IrType::I32
                    }
                    None => {
                        self.diags
                            .error(format!("undefined variable '{}'", name), Some(span));
                        IrType::I32
                    }
                };
                let expr_type = self.check_expr(expr);
                if !types_compatible(&expr_type, &var_type) {
                    self.diags.error(
                        format!(
                            "cannot assign {} to variable '{}' of type {}",
                            expr_type, name, var_type
                        ),
                        Some(expr.1),
                    );
                }
                IrType::Void
            }
        }
    }

    fn check_expr(&mut self, expr: &SpExpr) -> IrType {
        let span = expr.1;
        match &expr.0 {
            ExprKind::Int(_) => IrType::I32,
            ExprKind::Bool(_) => IrType::Bool,
            ExprKind::Ident(name) => match self.syms.lookup(name) {
                Some(SymType::Var(t)) => t.clone(),
                Some(SymType::Func { .. }) => {
                    self.diags
                        .error(format!("'{}' is a function, not a variable", name), Some(span));
                    IrType::I32
                }
                None => {
                    self.diags
                        .error(format!("undefined identifier '{}'", name), Some(span));
                    IrType::I32
                }
            },
            ExprKind::Binary(op, lhs, rhs) => {
                let lt = self.check_expr(lhs);
                let rt = self.check_expr(rhs);
                match op {
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
                        if !types_compatible_arith(&lt, &rt) {
                            self.diags.error(
                                format!("type mismatch: {} {} {}", lt, op, rt),
                                Some(span),
                            );
                        }
                        lt
                    }
                    BinaryOp::Eq | BinaryOp::Neq => {
                        if !types_compatible_arith(&lt, &rt) {
                            self.diags.error(
                                format!("cannot compare {} and {}", lt, rt),
                                Some(span),
                            );
                        }
                        IrType::Bool
                    }
                    BinaryOp::Lt | BinaryOp::Gt | BinaryOp::Le | BinaryOp::Ge => {
                        if !types_compatible_arith(&lt, &rt) {
                            self.diags.error(
                                format!("cannot compare {} and {}", lt, rt),
                                Some(span),
                            );
                        }
                        IrType::Bool
                    }
                    BinaryOp::And | BinaryOp::Or => {
                        if lt != IrType::Bool || rt != IrType::Bool {
                            self.diags.error(
                                format!("logical operators require bool operands"),
                                Some(span),
                            );
                        }
                        IrType::Bool
                    }
                }
            }
            ExprKind::Unary(op, expr) => {
                let t = self.check_expr(expr);
                match op {
                    UnaryOp::Neg => {
                        if !is_numeric(&t) {
                            self.diags
                                .error(format!("cannot negate {}", t), Some(span));
                        }
                        t
                    }
                    UnaryOp::Not => {
                        if t != IrType::Bool {
                            self.diags
                                .error(format!("cannot apply ! to {}", t), Some(span));
                        }
                        IrType::Bool
                    }
                }
            }
            ExprKind::Call(name, args) => {
                let func_type = match self.syms.lookup(name) {
                    Some(SymType::Func {
                        params,
                        return_type,
                    }) => (params.clone(), return_type.clone()),
                    Some(SymType::Var(_)) => {
                        self.diags
                            .error(format!("'{}' is a variable, not a function", name), Some(span));
                        return IrType::I32;
                    }
                    None => {
                        self.diags
                            .error(format!("undefined function '{}'", name), Some(span));
                        return IrType::I32;
                    }
                };
                let (param_types, return_type) = func_type;
                if args.len() != param_types.len() {
                    self.diags.error(
                        format!(
                            "function '{}' takes {} arguments, found {}",
                            name,
                            param_types.len(),
                            args.len()
                        ),
                        Some(span),
                    );
                    return IrType::I32;
                }
                for (i, arg) in args.iter().enumerate() {
                    let arg_type = self.check_expr(arg);
                    if !types_compatible(&arg_type, &param_types[i]) {
                        self.diags.error(
                            format!(
                                "argument {} of '{}': expected {}, found {}",
                                i + 1,
                                name,
                                param_types[i],
                                arg_type
                            ),
                            Some(arg.1),
                        );
                    }
                }
                return_type
            }
            ExprKind::Cast(type_, expr) => {
                let target_type = ast_type_to_ir(type_);
                let _src_type = self.check_expr(expr);
                target_type
            }
        }
    }
}

fn types_compatible(a: &IrType, b: &IrType) -> bool {
    a == b
}

fn types_compatible_arith(a: &IrType, b: &IrType) -> bool {
    is_numeric(a) && a == b
}

fn is_numeric(t: &IrType) -> bool {
    matches!(
        t,
        IrType::I32 | IrType::I64 | IrType::U32 | IrType::U64 | IrType::I8 | IrType::U8
    )
}

use std::fmt;

impl fmt::Display for IrType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            IrType::I32 => write!(f, "i32"),
            IrType::I64 => write!(f, "i64"),
            IrType::U32 => write!(f, "u32"),
            IrType::U64 => write!(f, "u64"),
            IrType::I8 => write!(f, "i8"),
            IrType::U8 => write!(f, "u8"),
            IrType::Bool => write!(f, "bool"),
            IrType::Void => write!(f, "void"),
            IrType::Ptr(inner) => write!(f, "ptr<{}>", inner),
        }
    }
}
