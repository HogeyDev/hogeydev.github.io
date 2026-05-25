use crate::ast::*;
use crate::diag::Diagnostics;
use crate::lexer::Lexer;
use crate::span::Span;
use crate::token::Token;

pub struct Parser<'a> {
    tokens: Vec<(Token, Span)>,
    pos: usize,
    diags: &'a mut Diagnostics,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, diags: &'a mut Diagnostics) -> Self {
        let mut lexer = Lexer::new(source, diags);
        let mut tokens = Vec::new();
        loop {
            let (tok, span) = lexer.next_token();
            let is_eof = tok == Token::EOF;
            tokens.push((tok, span));
            if is_eof {
                break;
            }
        }
        Parser {
            tokens,
            pos: 0,
            diags,
        }
    }

    fn peek(&self) -> &(Token, Span) {
        &self.tokens[self.pos]
    }

    fn peek_token(&self) -> &Token {
        &self.tokens[self.pos].0
    }

    fn peek_span(&self) -> Span {
        self.tokens[self.pos].1
    }

    fn advance(&mut self) -> (Token, Span) {
        let tok = self.tokens[self.pos].clone();
        self.pos += 1;
        tok
    }

    fn expect(&mut self, expected: &Token) -> Span {
        let (ref tok, span) = self.tokens[self.pos];
        if tok == expected {
            self.pos += 1;
            span
        } else {
            self.diags.error(
                format!("expected {}, found {}", expected, tok),
                Some(span),
            );
            span
        }
    }

    fn check(&self, tok: &Token) -> bool {
        self.peek_token() == tok
    }

    fn check_advance(&mut self, tok: &Token) -> bool {
        if self.check(tok) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn sync_to(&mut self, tokens: &[Token]) {
        while !tokens.contains(self.peek_token()) && !self.check(&Token::EOF) {
            self.advance();
        }
    }

    pub fn parse_program(&mut self) -> AstProgram {
        let mut decls = Vec::new();
        while !self.check(&Token::EOF) {
            match self.peek_token() {
                Token::Func => {
                    decls.push(AstDecl::Func(self.parse_func_decl()));
                }
                Token::Let => {
                    self.diags
                        .error("global variable declarations not supported yet", Some(self.peek_span()));
                    self.advance();
                    self.sync_to(&[Token::Func, Token::Let, Token::EOF]);
                }
                _ => {
                    self.diags
                        .error("expected top-level declaration", Some(self.peek_span()));
                    self.advance();
                }
            }
        }
        AstProgram { decls }
    }

    fn parse_func_decl(&mut self) -> AstFuncDecl {
        let start_span = self.peek_span();
        self.expect(&Token::Func);
        let (_, name_span) = self.advance();
        let name = match &self.tokens[self.pos - 1].0 {
            Token::Ident(s) => s.clone(),
            _ => {
                self.diags
                    .error("expected function name", Some(name_span));
                String::new()
            }
        };

        self.expect(&Token::LParen);

        let return_type = if self.check(&Token::RParen) {
            AstType::Prim(PrimType::Void)
        } else {
            self.parse_type()
        };

        let mut params = Vec::new();
        while self.check_advance(&Token::Comma) {
            let (_, name_span) = self.advance();
            let param_name = match &self.tokens[self.pos - 1].0 {
                Token::Ident(s) => s.clone(),
                _ => {
                    self.diags
                        .error("expected parameter name", Some(name_span));
                    String::new()
                }
            };
            self.expect(&Token::Colon);
            let param_type = self.parse_type();
            params.push(AstParam {
                name: param_name,
                name_span,
                type_: param_type,
            });
        }

        self.expect(&Token::RParen);
        self.expect(&Token::LBrace);

        let body = self.parse_block_body();

        let end_span = self.peek_span();
        AstFuncDecl {
            name,
            name_span,
            return_type,
            params,
            body,
            span: Span {
                start: start_span.start,
                end: end_span.end,
            },
        }
    }

    fn parse_block_body(&mut self) -> Vec<SpStmt> {
        let mut stmts = Vec::new();
        while !self.check(&Token::RBrace) && !self.check(&Token::EOF) {
            stmts.push(self.parse_statement());
        }
        self.expect(&Token::RBrace);
        stmts
    }

    fn parse_statement(&mut self) -> SpStmt {
        let start = self.peek_span();
        match self.peek_token() {
            Token::Return => {
                self.advance();
                let expr = if !self.check(&Token::Semicolon) {
                    let e = self.parse_expr(0);
                    self.expect(&Token::Semicolon);
                    Some(e)
                } else {
                    self.expect(&Token::Semicolon);
                    None
                };
                let span = Span {
                    start: start.start,
                    end: self.pos,
                };
                Spanned(StmtKind::Return(expr), span)
            }
            Token::Let => {
                self.advance();
                let (_, name_span) = self.advance();
                let name = match &self.tokens[self.pos - 1].0 {
                    Token::Ident(s) => s.clone(),
                    _ => {
                        self.diags
                            .error("expected variable name", Some(name_span));
                        String::new()
                    }
                };
                self.expect(&Token::Colon);
                let type_ = self.parse_type();
                let init = if self.check_advance(&Token::Assign) {
                    let e = self.parse_expr(0);
                    self.expect(&Token::Semicolon);
                    Some(e)
                } else {
                    self.expect(&Token::Semicolon);
                    None
                };
                let span = Span {
                    start: start.start,
                    end: self.pos,
                };
                Spanned(
                    StmtKind::VarDecl {
                        name,
                        type_,
                        init,
                    },
                    span,
                )
            }
            Token::If => self.parse_if(),
            Token::While => self.parse_while(),
            Token::LBrace => {
                self.advance();
                let stmts = self.parse_block_body();
                let span = Span {
                    start: start.start,
                    end: self.pos,
                };
                Spanned(StmtKind::Block(stmts), span)
            }
            _ => {
                let expr = self.parse_expr(0);
                if self.check(&Token::Assign) && matches!(expr.0, ExprKind::Ident(_)) {
                    self.advance();
                    let value = self.parse_expr(0);
                    self.expect(&Token::Semicolon);
                    let name = match &expr.0 {
                        ExprKind::Ident(s) => s.clone(),
                        _ => unreachable!(),
                    };
                    Spanned(
                        StmtKind::Assign(name, value),
                        Span {
                            start: start.start,
                            end: self.pos,
                        },
                    )
                } else {
                    self.expect(&Token::Semicolon);
                    Spanned(
                        StmtKind::Expr(expr),
                        Span {
                            start: start.start,
                            end: self.pos,
                        },
                    )
                }
            }
        }
    }

    fn parse_if(&mut self) -> SpStmt {
        let start = self.peek_span();
        self.advance();
        self.expect(&Token::LParen);
        let cond = self.parse_expr(0);
        self.expect(&Token::RParen);
        self.expect(&Token::LBrace);
        let then_body = self.parse_block_body();
        let else_body = if self.check_advance(&Token::Else) {
            if self.check(&Token::LBrace) {
                self.advance();
                Some(self.parse_block_body())
            } else if self.check(&Token::If) {
                // else if - wrap in a block
                let if_stmt = self.parse_if();
                Some(vec![if_stmt])
            } else {
                None
            }
        } else {
            None
        };
        Spanned(
            StmtKind::If(cond, then_body, else_body),
            Span {
                start: start.start,
                end: self.pos,
            },
        )
    }

    fn parse_while(&mut self) -> SpStmt {
        let start = self.peek_span();
        self.advance();
        self.expect(&Token::LParen);
        let cond = self.parse_expr(0);
        self.expect(&Token::RParen);
        self.expect(&Token::LBrace);
        let body = self.parse_block_body();
        Spanned(
            StmtKind::While(cond, body),
            Span {
                start: start.start,
                end: self.pos,
            },
        )
    }

    fn parse_expr(&mut self, min_bp: u8) -> SpExpr {
        let (tok, span) = self.advance();
        let ((), rbp) = prefix_bp(&tok).unwrap_or_else(|| {
            self.diags
                .error(format!("expected expression, found {}", tok), Some(span));
            ((), 0)
        });

        let (mut lhs, mut span) = self.parse_prefix(tok);

        loop {
            let (next_tok, next_span) = self.peek().clone();
            if let Some((lbp, rbp)) = infix_bp(&next_tok) {
                if lbp < min_bp {
                    break;
                }
                self.advance();
                match next_tok {
                    Token::LParen => {
                        let name = match lhs {
                            ExprKind::Ident(s) => s,
                            _ => {
                                self.diags
                                    .error("callee must be an identifier", Some(span));
                                String::new()
                            }
                        };
                        let mut args = vec![];
                        if !self.check(&Token::RParen) {
                            args.push(self.parse_expr(0));
                            while self.check_advance(&Token::Comma) {
                                args.push(self.parse_expr(0));
                            }
                        }
                        self.expect(&Token::RParen);
                        lhs = ExprKind::Call(name, args);
                        span = Span {
                            start: span.start,
                            end: self.tokens[self.pos - 1].1.end,
                        };
                    }
                    _ => {
                        let rhs = self.parse_expr(rbp);
                        let op_span = next_span;
                        let full_span = Span {
                            start: span.start,
                            end: rhs.1.end,
                        };
                        match next_tok {
                            Token::Plus => {
                                lhs = ExprKind::Binary(
                                    BinaryOp::Add,
                                    Box::new(Spanned(lhs, span)),
                                    Box::new(rhs),
                                );
                            }
                            Token::Minus => {
                                lhs = ExprKind::Binary(
                                    BinaryOp::Sub,
                                    Box::new(Spanned(lhs, span)),
                                    Box::new(rhs),
                                );
                            }
                            Token::Star => {
                                lhs = ExprKind::Binary(
                                    BinaryOp::Mul,
                                    Box::new(Spanned(lhs, span)),
                                    Box::new(rhs),
                                );
                            }
                            Token::Slash => {
                                lhs = ExprKind::Binary(
                                    BinaryOp::Div,
                                    Box::new(Spanned(lhs, span)),
                                    Box::new(rhs),
                                );
                            }
                            Token::Eq => {
                                lhs = ExprKind::Binary(
                                    BinaryOp::Eq,
                                    Box::new(Spanned(lhs, span)),
                                    Box::new(rhs),
                                );
                            }
                            Token::Neq => {
                                lhs = ExprKind::Binary(
                                    BinaryOp::Neq,
                                    Box::new(Spanned(lhs, span)),
                                    Box::new(rhs),
                                );
                            }
                            Token::Lt => {
                                lhs = ExprKind::Binary(
                                    BinaryOp::Lt,
                                    Box::new(Spanned(lhs, span)),
                                    Box::new(rhs),
                                );
                            }
                            Token::Gt => {
                                lhs = ExprKind::Binary(
                                    BinaryOp::Gt,
                                    Box::new(Spanned(lhs, span)),
                                    Box::new(rhs),
                                );
                            }
                            Token::Le => {
                                lhs = ExprKind::Binary(
                                    BinaryOp::Le,
                                    Box::new(Spanned(lhs, span)),
                                    Box::new(rhs),
                                );
                            }
                            Token::Ge => {
                                lhs = ExprKind::Binary(
                                    BinaryOp::Ge,
                                    Box::new(Spanned(lhs, span)),
                                    Box::new(rhs),
                                );
                            }
                            Token::AndAnd => {
                                lhs = ExprKind::Binary(
                                    BinaryOp::And,
                                    Box::new(Spanned(lhs, span)),
                                    Box::new(rhs),
                                );
                            }
                            Token::OrOr => {
                                lhs = ExprKind::Binary(
                                    BinaryOp::Or,
                                    Box::new(Spanned(lhs, span)),
                                    Box::new(rhs),
                                );
                            }
                            Token::Assign => {
                                let name = match lhs {
                                    ExprKind::Ident(s) => s,
                                    _ => {
                                        self.diags
                                            .error("assignment target must be an identifier", Some(span));
                                        String::new()
                                    }
                                };
                                lhs = ExprKind::Ident(name.clone());
                                lhs = ExprKind::Binary(
                                    BinaryOp::Add,
                                    Box::new(Spanned(lhs, span)),
                                    Box::new(rhs),
                                );
                                self.diags
                                    .error("assignment as expression not supported", Some(next_span));
                            }
                            _ => {
                                self.diags
                                    .error(format!("unexpected operator {}", next_tok), Some(next_span));
                            }
                        }
                        span = full_span;
                    }
                }
            } else {
                break;
            }
        }

        Spanned(lhs, span)
    }

    fn parse_prefix(&mut self, tok: Token) -> (ExprKind, Span) {
        let start = self.tokens[self.pos - 1].1;
        match tok {
            Token::IntLit(n) => {
                let span = self.tokens[self.pos - 1].1;
                (ExprKind::Int(n), span)
            }
            Token::True => {
                let span = self.tokens[self.pos - 1].1;
                (ExprKind::Bool(true), span)
            }
            Token::False => {
                let span = self.tokens[self.pos - 1].1;
                (ExprKind::Bool(false), span)
            }
            Token::Ident(s) => {
                let span = self.tokens[self.pos - 1].1;
                (ExprKind::Ident(s), span)
            }
            Token::Minus => {
                let Spanned(rhs, rhs_span) = self.parse_expr(15);
                let span = Span {
                    start: start.start,
                    end: rhs_span.end,
                };
                (ExprKind::Unary(UnaryOp::Neg, Box::new(Spanned(rhs, rhs_span))), span)
            }
            Token::Not => {
                let Spanned(rhs, rhs_span) = self.parse_expr(15);
                let span = Span {
                    start: start.start,
                    end: rhs_span.end,
                };
                (ExprKind::Unary(UnaryOp::Not, Box::new(Spanned(rhs, rhs_span))), span)
            }
            Token::LParen => {
                let expr = self.parse_expr(0);
                self.expect(&Token::RParen);
                (expr.0, expr.1)
            }
            _ => {
                // If it's a type keyword, try parsing a cast expression
                if is_type_token(&tok) {
                    let type_ = self.parse_type();
                    let Spanned(rhs, rhs_span) = self.parse_expr(15);
                    let span = Span {
                        start: start.start,
                        end: rhs_span.end,
                    };
                    (ExprKind::Cast(type_, Box::new(Spanned(rhs, rhs_span))), span)
                } else {
                    self.diags
                        .error(format!("expected expression, found {}", tok), Some(start));
                    (ExprKind::Int(0), start)
                }
            }
        }
    }

    fn parse_type(&mut self) -> AstType {
        let tok = self.peek_token().clone();
        match tok {
            Token::I32 => {
                self.advance();
                AstType::Prim(PrimType::I32)
            }
            Token::I64 => {
                self.advance();
                AstType::Prim(PrimType::I64)
            }
            Token::U32 => {
                self.advance();
                AstType::Prim(PrimType::U32)
            }
            Token::U64 => {
                self.advance();
                AstType::Prim(PrimType::U64)
            }
            Token::I8 => {
                self.advance();
                AstType::Prim(PrimType::I8)
            }
            Token::U8 => {
                self.advance();
                AstType::Prim(PrimType::U8)
            }
            Token::Bool => {
                self.advance();
                AstType::Prim(PrimType::Bool)
            }
            Token::Void => {
                self.advance();
                AstType::Prim(PrimType::Void)
            }
            Token::Star => {
                self.advance();
                let inner = self.parse_type();
                AstType::Ptr(Box::new(inner))
            }
            Token::LBracket => {
                self.advance();
                let inner = self.parse_type();
                let size = if self.check_advance(&Token::Semicolon) {
                    let (tok, _) = self.advance();
                    if let Token::IntLit(n) = tok {
                        Some(n as usize)
                    } else {
                        None
                    }
                } else {
                    None
                };
                self.expect(&Token::RBracket);
                AstType::Array(Box::new(inner), size)
            }
            Token::Ident(s) => {
                self.advance();
                AstType::Named(s)
            }
            _ => {
                self.diags
                    .error(format!("expected type, found {}", tok), Some(self.peek_span()));
                AstType::Prim(PrimType::I32)
            }
        }
    }
}

fn prefix_bp(tok: &Token) -> Option<((), u8)> {
    match tok {
        Token::IntLit(_) | Token::Ident(_) | Token::True | Token::False | Token::LParen => {
            Some(((), 0))
        }
        Token::Minus | Token::Not => Some(((), 15)),
        _ => {
            if is_type_token(tok) {
                Some(((), 0))
            } else {
                None
            }
        }
    }
}

fn infix_bp(tok: &Token) -> Option<(u8, u8)> {
    match tok {
        Token::OrOr => Some((1, 2)),
        Token::AndAnd => Some((3, 4)),
        Token::Eq | Token::Neq => Some((5, 6)),
        Token::Lt | Token::Gt | Token::Le | Token::Ge => Some((7, 8)),
        Token::Plus | Token::Minus => Some((10, 11)),
        Token::Star | Token::Slash => Some((20, 21)),
        Token::LParen => Some((25, 25)),
        _ => None,
    }
}

fn is_type_token(tok: &Token) -> bool {
    matches!(
        tok,
        Token::I32
            | Token::I64
            | Token::U32
            | Token::U64
            | Token::I8
            | Token::U8
            | Token::Bool
            | Token::Void
    )
}
