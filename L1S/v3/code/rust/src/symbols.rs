use std::collections::HashMap;
use crate::ir::IrType;

#[derive(Clone, Debug)]
pub enum SymbolKind {
    Func(Vec<IrType>, IrType),
    Var(IrType),
}

#[derive(Clone, Debug)]
pub struct Symbol {
    pub type_: IrType,
    pub kind: SymbolKind,
}

pub struct SymbolTable {
    scopes: Vec<HashMap<String, Symbol>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self { scopes: vec![HashMap::new()] }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn insert(&mut self, name: String, sym: Symbol) {
        self.scopes.last_mut().unwrap().insert(name, sym);
    }

    pub fn insert_var(&mut self, name: &str, type_: IrType) {
        self.insert(name.to_string(), Symbol {
            type_: type_.clone(),
            kind: SymbolKind::Var(type_),
        });
    }

    pub fn insert_func(&mut self, name: &str, param_types: Vec<IrType>, return_type: IrType) {
        let rt = return_type.clone();
        self.insert(name.to_string(), Symbol {
            type_: return_type,
            kind: SymbolKind::Func(param_types, rt),
        });
    }

    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(sym) = scope.get(name) {
                return Some(sym);
            }
        }
        None
    }
}
