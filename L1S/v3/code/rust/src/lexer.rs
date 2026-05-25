use crate::span::Span;
use crate::token::{Token, SpannedToken, keyword_token};
use crate::diag::Diagnostics;

pub struct Lexer {
    source: Vec<char>,
    pos: usize,
    diag: Diagnostics,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Self { source: source.chars().collect(), pos: 0, diag: Diagnostics::new() }
    }

    pub fn tokenize(&mut self) -> (Vec<SpannedToken>, Diagnostics) {
        let mut tokens = vec![];
        loop {
            self.skip_whitespace();
            if self.pos >= self.source.len() { break; }
            let start = self.pos;
            let ch = self.source[self.pos];
            let token = match ch {
                '(' => { self.pos += 1; Token::OpenParen }
                ')' => { self.pos += 1; Token::CloseParen }
                '{' => { self.pos += 1; Token::OpenBrace }
                '}' => { self.pos += 1; Token::CloseBrace }
                '[' => { self.pos += 1; Token::OpenBracket }
                ']' => { self.pos += 1; Token::CloseBracket }
                ':' => { self.pos += 1; Token::Colon }
                ',' => { self.pos += 1; Token::Comma }
                ';' => { self.pos += 1; Token::Semicolon }
                '.' => { self.pos += 1; Token::Dot }
                '@' => { self.pos += 1; Token::At }
                '+' => { self.pos += 1; Token::Plus }
                '-' => {
                    if self.pos + 1 < self.source.len() && self.source[self.pos + 1] == '>' {
                        self.pos += 2; Token::Arrow
                    } else { self.pos += 1; Token::Minus }
                }
                '*' => { self.pos += 1; Token::Star }
                '/' => {
                    if self.pos + 1 < self.source.len() && self.source[self.pos + 1] == '/' {
                        self.skip_comment();
                        continue;
                    }
                    self.pos += 1; Token::Slash
                }
                '!' => {
                    if self.pos + 1 < self.source.len() && self.source[self.pos + 1] == '=' {
                        self.pos += 2; Token::BangEq
                    } else { self.pos += 1; Token::Bang }
                }
                '=' => {
                    if self.pos + 1 < self.source.len() && self.source[self.pos + 1] == '=' {
                        self.pos += 2; Token::EqEq
                    } else { self.pos += 1; Token::Eq }
                }
                '<' => {
                    if self.pos + 1 < self.source.len() && self.source[self.pos + 1] == '=' {
                        self.pos += 2; Token::LtEq
                    } else { self.pos += 1; Token::Lt }
                }
                '>' => {
                    if self.pos + 1 < self.source.len() && self.source[self.pos + 1] == '=' {
                        self.pos += 2; Token::GtEq
                    } else { self.pos += 1; Token::Gt }
                }
                '&' => {
                    if self.pos + 1 < self.source.len() && self.source[self.pos + 1] == '&' {
                        self.pos += 2; Token::AndAnd
                    } else { self.pos += 1; Token::AndAnd }
                }
                '|' => {
                    if self.pos + 1 < self.source.len() && self.source[self.pos + 1] == '|' {
                        self.pos += 2; Token::PipePipe
                    } else { self.pos += 1; Token::PipePipe }
                }
                '"' => self.lex_string(),
                '0'..='9' => self.lex_number(),
                'a'..='z' | 'A'..='Z' | '_' => self.lex_identifier_or_keyword(),
                _ => {
                    self.diag.error(Span::new(start, start + 1), format!("unexpected character '{}'", ch));
                    self.pos += 1; continue;
                }
            };
            tokens.push((token, Span::new(start, self.pos)));
        }
        tokens.push((Token::Eof, Span::new(self.pos, self.pos)));
        (tokens, std::mem::replace(&mut self.diag, Diagnostics::new()))
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.source.len() && self.source[self.pos].is_ascii_whitespace() {
            self.pos += 1;
        }
    }

    fn skip_comment(&mut self) {
        while self.pos < self.source.len() && self.source[self.pos] != '\n' {
            self.pos += 1;
        }
    }

    fn lex_identifier_or_keyword(&mut self) -> Token {
        let start = self.pos;
        while self.pos < self.source.len() && (self.source[self.pos].is_alphanumeric() || self.source[self.pos] == '_') {
            self.pos += 1;
        }
        let word: String = self.source[start..self.pos].iter().collect();
        keyword_token(&word).unwrap_or(Token::Ident(word))
    }

    fn lex_number(&mut self) -> Token {
        let start = self.pos;
        while self.pos < self.source.len() && self.source[self.pos].is_ascii_digit() {
            self.pos += 1;
        }
        Token::NumLiteral(self.source[start..self.pos].iter().collect())
    }

    fn lex_string(&mut self) -> Token {
        let start = self.pos;
        self.pos += 1;
        while self.pos < self.source.len() && self.source[self.pos] != '"' {
            if self.source[self.pos] == '\\' { self.pos += 1; }
            self.pos += 1;
        }
        if self.pos < self.source.len() { self.pos += 1; }
        Token::StrLiteral(self.source[start..self.pos].iter().collect())
    }
}
