use crate::span::Span;
use crate::token::{Token, SpannedToken};
use crate::diag::Diagnostics;
use crate::ast::*;

pub struct Parser {
    tokens: Vec<SpannedToken>,
    pos: usize,
    pub diag: Diagnostics,
}

impl Parser {
    pub fn new(tokens: Vec<SpannedToken>) -> Self {
        Self { tokens, pos: 0, diag: Diagnostics::new() }
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.pos].0
    }

    fn peek_span(&self) -> Span {
        self.tokens[self.pos].1
    }

    fn advance(&mut self) -> Token {
        let tok = self.tokens[self.pos].0.clone();
        self.pos += 1;
        tok
    }

    fn check(&self, tok: Token) -> bool {
        *self.peek() == tok
    }

    fn expect(&mut self, tok: Token) -> Span {
        if *self.peek() == tok {
            let span = self.peek_span();
            self.advance();
            span
        } else {
            let span = self.peek_span();
            self.diag.error(span, format!("expected {:?}, got {:?}", tok, self.peek()));
            span
        }
    }

    fn expect_ident(&mut self) -> String {
        if let Token::Ident(name) = self.peek() {
            let name = name.clone();
            self.advance();
            name
        } else {
            self.diag.error(self.peek_span(), "expected identifier");
            String::new()
        }
    }

    pub fn parse_program(&mut self) -> AstProgram {
        let mut decls = vec![];
        while !self.check(Token::Eof) {
            if let Some(decl) = self.parse_declaration() {
                decls.push(decl);
            } else {
                self.advance();
            }
        }
        AstProgram { decls }
    }

    fn parse_declaration(&mut self) -> Option<AstDecl> {
        let pub_ = if self.check(Token::Pub) { self.advance(); true } else { false };
        if self.check(Token::Func) {
            Some(AstDecl::Func(self.parse_func_decl(pub_)))
        } else if self.check(Token::Static) {
            self.advance();
            let sname = self.expect_ident();
            self.expect(Token::Colon);
            let _stype = self.parse_type();
            self.expect(Token::Semicolon);
            Some(AstDecl::Struct(AstStructDecl { name: sname, fields: vec![] }))
        } else {
            self.diag.error(self.peek_span(), "expected declaration");
            None
        }
    }

    fn parse_func_decl(&mut self, pub_: bool) -> AstFuncDecl {
        self.expect(Token::Func);
        let name = self.expect_ident();
        self.expect(Token::OpenParen);
        let return_type = self.parse_type();
        let mut params = vec![];
        if self.check(Token::Comma) {
            self.advance();
            while !self.check(Token::CloseParen) && !self.check(Token::Eof) {
                let param_name = self.expect_ident();
                self.expect(Token::Colon);
                let param_type = self.parse_type();
                params.push(AstParam { name: param_name, type_: param_type });
                if self.check(Token::Comma) { self.advance(); } else { break; }
            }
        }
        self.expect(Token::CloseParen);
        let body = self.parse_block();
        AstFuncDecl { pub_, name, return_type, params, body }
    }

    fn parse_block(&mut self) -> AstBlock {
        self.expect(Token::OpenBrace);
        let mut stmts = vec![];
        while !self.check(Token::CloseBrace) && !self.check(Token::Eof) {
            stmts.push(self.parse_statement());
        }
        self.expect(Token::CloseBrace);
        AstBlock { stmts }
    }

    fn parse_statement(&mut self) -> AstStmt {
        if self.check(Token::Return) {
            self.advance();
            let expr = if !self.check(Token::Semicolon) {
                let e = self.parse_expr();
                self.expect(Token::Semicolon);
                Some(e)
            } else {
                self.expect(Token::Semicolon);
                None
            };
            AstStmt::Return(expr)
        } else if self.check(Token::Let) {
            self.advance();
            let name = self.expect_ident();
            self.expect(Token::Colon);
            let type_ = self.parse_type();
            let init = if self.check(Token::Eq) {
                self.advance();
                Some(self.parse_expr())
            } else { None };
            self.expect(Token::Semicolon);
            AstStmt::VarDecl(AstVarDecl { name, type_, init })
        } else if self.check(Token::If) {
            self.advance();
            self.expect(Token::OpenParen);
            let cond = self.parse_expr();
            self.expect(Token::CloseParen);
            let then_block = self.parse_block();
            let else_branch = if self.check(Token::Else) {
                self.advance();
                if self.check(Token::If) {
                    Some(Box::new(self.parse_statement()))
                } else {
                    let block = self.parse_block();
                    Some(Box::new(AstStmt::Block(block)))
                }
            } else { None };
            AstStmt::If(AstIf { cond, then_block, else_branch })
        } else if self.check(Token::While) {
            self.advance();
            self.expect(Token::OpenParen);
            let cond = self.parse_expr();
            self.expect(Token::CloseParen);
            let body = self.parse_block();
            AstStmt::While(AstWhile { cond, body })
        } else if self.check(Token::OpenBrace) {
            AstStmt::Block(self.parse_block())
        } else {
            let expr = self.parse_expr();
            self.expect(Token::Semicolon);
            AstStmt::Expr(expr)
        }
    }

    fn parse_expr(&mut self) -> AstExpr {
        self.parse_expr_bp(0)
    }

    fn parse_expr_bp(&mut self, min_bp: u8) -> AstExpr {
        let mut lhs = self.parse_prefix();

        loop {
            let infix = self.peek_infix_bp();
            match infix {
                Some((InfixKind::Binary(_), l_bp, r_bp)) if l_bp >= min_bp => {
                    self.advance();
                    let rhs = self.parse_expr_bp(r_bp);
                    let op = match infix.unwrap().0 {
                        InfixKind::Binary(o) => o,
                        _ => unreachable!(),
                    };
                    lhs = AstExpr::Binary(op, Box::new(lhs), Box::new(rhs));
                }
                Some((InfixKind::Call, l_bp, r_bp)) if l_bp >= min_bp => {
                    self.advance();
                    let mut args = vec![];
                    if !self.check(Token::CloseParen) {
                        args.push(self.parse_expr());
                        while self.check(Token::Comma) {
                            self.advance();
                            args.push(self.parse_expr());
                        }
                    }
                    self.expect(Token::CloseParen);
                    if let AstExpr::Ident(name) = lhs {
                        lhs = AstExpr::Call(name, args);
                    } else {
                        self.diag.error(self.peek_span(), "callee must be identifier");
                    }
                }
                Some((InfixKind::Assign, _, r_bp)) if r_bp >= min_bp => {
                    self.advance();
                    let rhs = self.parse_expr_bp(r_bp);
                    lhs = AstExpr::Assign(Box::new(lhs), Box::new(rhs));
                }
                _ => break,
            }
        }
        lhs
    }

    fn parse_prefix(&mut self) -> AstExpr {
        match self.peek().clone() {
            Token::NumLiteral(n) => {
                self.advance();
                AstExpr::Int(n.parse().unwrap_or(0))
            }
            Token::True => { self.advance(); AstExpr::Bool(true) }
            Token::False => { self.advance(); AstExpr::Bool(false) }
            Token::Minus => {
                self.advance();
                let expr = self.parse_expr_bp(8);
                AstExpr::Unary(UnaryOp::Neg, Box::new(expr))
            }
            Token::Bang => {
                self.advance();
                let expr = self.parse_expr_bp(8);
                AstExpr::Unary(UnaryOp::Not, Box::new(expr))
            }
            Token::OpenParen => {
                self.advance();
                let expr = self.parse_expr();
                self.expect(Token::CloseParen);
                expr
            }
            Token::Ident(name) => {
                self.advance();
                if name == "cast" && self.check(Token::OpenParen) {
                    self.advance();
                    let type_ = self.parse_type();
                    self.expect(Token::Comma);
                    let expr = self.parse_expr();
                    self.expect(Token::CloseParen);
                    AstExpr::Cast(Box::new(type_), Box::new(expr))
                } else {
                    AstExpr::Ident(name)
                }
            }
            _ => {
                self.diag.error(self.peek_span(), "expected expression");
                self.advance();
                AstExpr::Int(0)
            }
        }
    }

    fn peek_infix_bp(&self) -> Option<(InfixKind, u8, u8)> {
        match self.peek() {
            Token::Eq => Some((InfixKind::Assign, 0, 1)),
            Token::PipePipe => Some((InfixKind::Binary(BinaryOp::Or), 2, 3)),
            Token::AndAnd => Some((InfixKind::Binary(BinaryOp::And), 3, 4)),
            Token::EqEq => Some((InfixKind::Binary(BinaryOp::Eq), 4, 5)),
            Token::BangEq => Some((InfixKind::Binary(BinaryOp::Neq), 4, 5)),
            Token::Lt => Some((InfixKind::Binary(BinaryOp::Lt), 5, 6)),
            Token::Gt => Some((InfixKind::Binary(BinaryOp::Gt), 5, 6)),
            Token::LtEq => Some((InfixKind::Binary(BinaryOp::Le), 5, 6)),
            Token::GtEq => Some((InfixKind::Binary(BinaryOp::Ge), 5, 6)),
            Token::Plus => Some((InfixKind::Binary(BinaryOp::Add), 6, 7)),
            Token::Minus => Some((InfixKind::Binary(BinaryOp::Sub), 6, 7)),
            Token::Star => Some((InfixKind::Binary(BinaryOp::Mul), 7, 8)),
            Token::Slash => Some((InfixKind::Binary(BinaryOp::Div), 7, 8)),
            Token::OpenParen => Some((InfixKind::Call, 9, 9)),
            _ => None,
        }
    }

    pub fn parse_type(&mut self) -> AstType {
        match self.peek() {
            Token::I32 => { self.advance(); AstType::Prim(PrimType::I32) }
            Token::I64 => { self.advance(); AstType::Prim(PrimType::I64) }
            Token::U32 => { self.advance(); AstType::Prim(PrimType::U32) }
            Token::U64 => { self.advance(); AstType::Prim(PrimType::U64) }
            Token::I8 => { self.advance(); AstType::Prim(PrimType::I8) }
            Token::U8 => { self.advance(); AstType::Prim(PrimType::U8) }
            Token::Bool => { self.advance(); AstType::Prim(PrimType::Bool) }
            Token::Void => { self.advance(); AstType::Prim(PrimType::Void) }
            Token::Char => { self.advance(); AstType::Prim(PrimType::Char) }
            Token::Isize => { self.advance(); AstType::Prim(PrimType::Isize) }
            Token::Usize => { self.advance(); AstType::Prim(PrimType::Usize) }
            Token::At => {
                self.advance();
                let inner = self.parse_type();
                AstType::Ptr(Box::new(inner))
            }
            Token::OpenBracket => {
                self.advance();
                let size = self.parse_expr();
                self.expect(Token::CloseBracket);
                let elem = self.parse_type();
                AstType::Array(Box::new(size), Box::new(elem))
            }
            Token::Ident(name) => {
                let name = name.clone();
                self.advance();
                AstType::Named(name)
            }
            _ => {
                self.diag.error(self.peek_span(), "expected type");
                self.advance();
                AstType::Prim(PrimType::I32)
            }
        }
    }
}

enum InfixKind {
    Binary(BinaryOp),
    Call,
    Assign,
}


