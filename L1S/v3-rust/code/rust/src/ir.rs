pub type VReg = usize;

#[derive(Clone, Debug, PartialEq)]
pub enum IrType {
    I32,
    I64,
    U32,
    U64,
    I8,
    U8,
    Bool,
    Void,
    Ptr(Box<IrType>),
}

#[derive(Clone, Debug)]
pub struct IrModule {
    pub funcs: Vec<IrFunction>,
}

impl IrModule {
    pub fn new() -> Self {
        IrModule { funcs: Vec::new() }
    }
}

#[derive(Clone, Debug)]
pub struct IrFunction {
    pub name: String,
    pub params: Vec<IrParam>,
    pub return_type: IrType,
    pub blocks: Vec<IrBlock>,
    pub num_vregs: usize,
    pub num_var_slots: usize,
}

impl IrFunction {
    pub fn new(name: String, return_type: IrType) -> Self {
        IrFunction {
            name,
            params: Vec::new(),
            return_type,
            blocks: Vec::new(),
            num_vregs: 0,
            num_var_slots: 0,
        }
    }
}

#[derive(Clone, Debug)]
pub struct IrParam {
    pub name: String,
    pub vreg: VReg,
    pub type_: IrType,
}

#[derive(Clone, Debug)]
pub struct IrBlock {
    pub label: String,
    pub instrs: Vec<IrInstr>,
    pub terminator: IrTerminator,
}

impl IrBlock {
    pub fn new(label: String) -> Self {
        IrBlock {
            label,
            instrs: Vec::new(),
            terminator: IrTerminator::Unreachable,
        }
    }
}

#[derive(Clone, Debug)]
pub enum IrInstr {
    Phi(VReg, Vec<(VReg, String)>),
    Const(VReg, i64, IrType),
    Add(VReg, VReg, VReg),
    Sub(VReg, VReg, VReg),
    Mul(VReg, VReg, VReg),
    Div(VReg, VReg, VReg),
    Eq(VReg, VReg, VReg),
    Neq(VReg, VReg, VReg),
    Lt(VReg, VReg, VReg),
    Gt(VReg, VReg, VReg),
    Le(VReg, VReg, VReg),
    Ge(VReg, VReg, VReg),
    Not(VReg, VReg),
    Neg(VReg, VReg),
    And(VReg, VReg, VReg),
    Or(VReg, VReg, VReg),
    Load(VReg, VReg, IrType),
    Store(VReg, VReg, IrType),
    LoadStack(VReg, usize, IrType),
    StoreStack(VReg, usize, IrType),
    Alloc(VReg, IrType),
    Call(VReg, String, Vec<VReg>, IrType),
    Copy(VReg, VReg),
}

impl IrInstr {
    pub fn dest(&self) -> Option<VReg> {
        use IrInstr::*;
        match self {
            Phi(d, _)
            | Const(d, _, _)
            | Add(d, _, _)
            | Sub(d, _, _)
            | Mul(d, _, _)
            | Div(d, _, _)
            | Eq(d, _, _)
            | Neq(d, _, _)
            | Lt(d, _, _)
            | Gt(d, _, _)
            | Le(d, _, _)
            | Ge(d, _, _)
            | Not(d, _)
            | Neg(d, _)
            | And(d, _, _)
            | Or(d, _, _)
            | Load(d, _, _)
            | LoadStack(d, _, _)
            | Alloc(d, _)
            | Call(d, _, _, _)
            | Copy(d, _) => Some(*d),
            Store(_, _, _) | StoreStack(_, _, _) => None,
        }
    }

    pub fn uses(&self) -> Vec<VReg> {
        use IrInstr::*;
        match self {
            Phi(_, ops) => ops.iter().map(|(v, _)| *v).collect(),
            Const(_, _, _) => vec![],
            Add(_, a, b)
            | Sub(_, a, b)
            | Mul(_, a, b)
            | Div(_, a, b)
            | Eq(_, a, b)
            | Neq(_, a, b)
            | Lt(_, a, b)
            | Gt(_, a, b)
            | Le(_, a, b)
            | Ge(_, a, b)
            | And(_, a, b)
            | Or(_, a, b) => vec![*a, *b],
            Not(_, a) | Neg(_, a) | Copy(_, a) => vec![*a],
            Load(_, addr, _) => vec![*addr],
            LoadStack(_, _, _) => vec![],
            Store(val, addr, _) => vec![*val, *addr],
            StoreStack(val, _, _) => vec![*val],
            Alloc(_, _) => vec![],
            Call(_, _, args, _) => args.clone(),
        }
    }

    pub fn is_side_effect(&self) -> bool {
        matches!(self, IrInstr::Store(_, _, _) | IrInstr::StoreStack(_, _, _) | IrInstr::Call(_, _, _, _))
    }

    pub fn is_copy(&self) -> bool {
        matches!(self, IrInstr::Copy(_, _))
    }
}

#[derive(Clone, Debug)]
pub enum IrTerminator {
    Ret(Option<VReg>),
    Br(String),
    BrCond(VReg, String, String),
    Unreachable,
}
