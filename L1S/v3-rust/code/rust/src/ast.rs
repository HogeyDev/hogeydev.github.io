use crate::span::Span;
use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum PrimType {
    I32,
    I64,
    U32,
    U64,
    I8,
    U8,
    Bool,
    Void,
}

#[derive(Clone, Debug, PartialEq)]
pub enum AstType {
    Prim(PrimType),
    Named(String),
    Ptr(Box<AstType>),
    Array(Box<AstType>, Option<usize>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
    And,
    Or,
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Clone, Debug)]
pub struct Spanned<T>(pub T, pub Span);

pub type SpExpr = Spanned<ExprKind>;
pub type SpStmt = Spanned<StmtKind>;

#[derive(Clone, Debug)]
pub enum ExprKind {
    Int(i64),
    Bool(bool),
    Ident(String),
    Binary(BinaryOp, Box<SpExpr>, Box<SpExpr>),
    Unary(UnaryOp, Box<SpExpr>),
    Call(String, Vec<SpExpr>),
    Cast(AstType, Box<SpExpr>),
}

#[derive(Clone, Debug)]
pub enum StmtKind {
    Return(Option<SpExpr>),
    VarDecl {
        name: String,
        type_: AstType,
        init: Option<SpExpr>,
    },
    If(SpExpr, Vec<SpStmt>, Option<Vec<SpStmt>>),
    While(SpExpr, Vec<SpStmt>),
    Block(Vec<SpStmt>),
    Expr(SpExpr),
    Assign(String, SpExpr),
}

#[derive(Clone, Debug)]
pub struct AstParam {
    pub name: String,
    pub name_span: Span,
    pub type_: AstType,
}

#[derive(Clone, Debug)]
pub struct AstFuncDecl {
    pub name: String,
    pub name_span: Span,
    pub return_type: AstType,
    pub params: Vec<AstParam>,
    pub body: Vec<SpStmt>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum AstDecl {
    Func(AstFuncDecl),
}

#[derive(Clone, Debug)]
pub struct AstProgram {
    pub decls: Vec<AstDecl>,
}

impl fmt::Display for PrimType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PrimType::I32 => write!(f, "i32"),
            PrimType::I64 => write!(f, "i64"),
            PrimType::U32 => write!(f, "u32"),
            PrimType::U64 => write!(f, "u64"),
            PrimType::I8 => write!(f, "i8"),
            PrimType::U8 => write!(f, "u8"),
            PrimType::Bool => write!(f, "bool"),
            PrimType::Void => write!(f, "void"),
        }
    }
}

impl fmt::Display for AstType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AstType::Prim(p) => write!(f, "{}", p),
            AstType::Named(s) => write!(f, "{}", s),
            AstType::Ptr(t) => write!(f, "ptr<{}>", t),
            AstType::Array(t, Some(n)) => write!(f, "[{}; {}]", t, n),
            AstType::Array(t, None) => write!(f, "[{}]", t),
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Sub => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::Eq => write!(f, "=="),
            BinaryOp::Neq => write!(f, "!="),
            BinaryOp::Lt => write!(f, "<"),
            BinaryOp::Gt => write!(f, ">"),
            BinaryOp::Le => write!(f, "<="),
            BinaryOp::Ge => write!(f, ">="),
            BinaryOp::And => write!(f, "&&"),
            BinaryOp::Or => write!(f, "||"),
        }
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnaryOp::Neg => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
        }
    }
}
