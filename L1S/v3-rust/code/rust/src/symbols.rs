use crate::ast::{AstType, PrimType};
use crate::ir::IrType;
use std::collections::HashMap;

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
        },
        AstType::Named(_) => IrType::I32,
        AstType::Ptr(inner) => IrType::Ptr(Box::new(ast_type_to_ir(inner))),
        AstType::Array(inner, _) => IrType::Ptr(Box::new(ast_type_to_ir(inner))),
    }
}

pub fn ir_type_size(t: &IrType) -> usize {
    match t {
        IrType::I32 | IrType::U32 => 4,
        IrType::I64 | IrType::U64 => 8,
        IrType::I8 | IrType::U8 => 1,
        IrType::Bool => 1,
        IrType::Void => 0,
        IrType::Ptr(_) => 8,
    }
}

#[derive(Clone, Debug)]
pub enum SymType {
    Func {
        params: Vec<IrType>,
        return_type: IrType,
    },
    Var(IrType),
}

#[derive(Clone, Debug)]
pub struct SymbolTable {
    scopes: Vec<HashMap<String, SymType>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn insert(&mut self, name: String, sym: SymType) -> bool {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(&name) {
                return false;
            }
            scope.insert(name, sym);
            true
        } else {
            false
        }
    }

    pub fn lookup(&self, name: &str) -> Option<&SymType> {
        for scope in self.scopes.iter().rev() {
            if let Some(sym) = scope.get(name) {
                return Some(sym);
            }
        }
        None
    }

    pub fn lookup_in_current_scope(&self, name: &str) -> Option<&SymType> {
        self.scopes.last().and_then(|s| s.get(name))
    }
}
