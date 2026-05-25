pub type VReg = usize;

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum IrType {
    I32, I64, U32, U64, I8, U8, Bool, Void,
    Ptr(usize),
}

#[derive(Clone, Debug)]
pub struct IrModule {
    pub funcs: Vec<IrFunction>,
}

#[derive(Clone, Debug)]
pub struct IrFunction {
    pub name: String,
    pub param_types: Vec<IrType>,
    pub param_vregs: Vec<VReg>,
    pub return_type: IrType,
    pub blocks: Vec<IrBlock>,
    pub num_vregs: usize,
}

#[derive(Clone, Debug)]
pub struct IrBlock {
    pub label: String,
    pub instrs: Vec<IrInstr>,
    pub terminator: Option<IrTerminator>,
}

#[derive(Clone, Debug)]
pub enum IrInstr {
    Const(VReg, i64, IrType),
    Mov(VReg, VReg),
    Add(VReg, VReg, VReg),
    Sub(VReg, VReg, VReg),
    Mul(VReg, VReg, VReg),
    Div(VReg, VReg, VReg),
    Eq(VReg, VReg, VReg),
    Lt(VReg, VReg, VReg),
    And(VReg, VReg, VReg),
    Or(VReg, VReg, VReg),
    Not(VReg, VReg),
    Neg(VReg, VReg),
    Call(VReg, String, Vec<VReg>, IrType),
}

#[derive(Clone, Debug)]
pub enum IrTerminator {
    Ret(Option<VReg>),
    Br(String),
    BrCond(VReg, String, String),
}
