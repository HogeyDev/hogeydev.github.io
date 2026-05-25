pub struct AstProgram {
    pub decls: Vec<AstDecl>,
}

pub enum AstDecl {
    Func(AstFuncDecl),
    Struct(AstStructDecl),
    Enum(AstEnumDecl),
}

pub struct AstFuncDecl {
    pub pub_: bool,
    pub name: String,
    pub return_type: AstType,
    pub params: Vec<AstParam>,
    pub body: AstBlock,
}

pub struct AstParam {
    pub name: String,
    pub type_: AstType,
}

pub struct AstBlock {
    pub stmts: Vec<AstStmt>,
}

pub enum AstStmt {
    Return(Option<AstExpr>),
    VarDecl(AstVarDecl),
    If(AstIf),
    While(AstWhile),
    Block(AstBlock),
    Expr(AstExpr),
}

pub struct AstVarDecl {
    pub name: String,
    pub type_: AstType,
    pub init: Option<AstExpr>,
}

pub struct AstIf {
    pub cond: AstExpr,
    pub then_block: AstBlock,
    pub else_branch: Option<Box<AstStmt>>,
}

pub struct AstWhile {
    pub cond: AstExpr,
    pub body: AstBlock,
}

pub enum AstExpr {
    Int(i64),
    Bool(bool),
    Ident(String),
    Binary(BinaryOp, Box<AstExpr>, Box<AstExpr>),
    Unary(UnaryOp, Box<AstExpr>),
    Call(String, Vec<AstExpr>),
    Cast(Box<AstType>, Box<AstExpr>),
    Assign(Box<AstExpr>, Box<AstExpr>),
}

pub enum BinaryOp { Add, Sub, Mul, Div, Eq, Neq, Lt, Gt, Le, Ge, And, Or }

pub enum UnaryOp { Neg, Not }

pub enum AstType {
    Prim(PrimType),
    Named(String),
    Ptr(Box<AstType>),
    Array(Box<AstExpr>, Box<AstType>),
}

#[derive(Clone, Copy, PartialEq)]
pub enum PrimType {
    I32, I64, U32, U64, I8, U8, Bool, Void, Char, Usize, Isize,
}

pub struct AstStructDecl {
    pub name: String,
    pub fields: Vec<AstField>,
}

pub struct AstField {
    pub name: String,
    pub type_: AstType,
}

pub struct AstEnumDecl {
    pub name: String,
    pub variants: Vec<String>,
}
