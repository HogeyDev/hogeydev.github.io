use crate::diag::Diagnostics;
use crate::span::Span;
use crate::token::{keyword_token, Token};

pub struct Lexer<'a> {
    source: &'a str,
    chars: Vec<char>,
    pos: usize,
    diags: &'a mut Diagnostics,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str, diags: &'a mut Diagnostics) -> Self {
        let chars: Vec<char> = source.chars().collect();
        Lexer {
            source,
            chars,
            pos: 0,
            diags,
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn peek_next(&self) -> Option<char> {
        self.chars.get(self.pos + 1).copied()
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.chars.get(self.pos).copied();
        if c.is_some() {
            self.pos += 1;
        }
        c
    }

    fn span(&self, start: usize) -> Span {
        Span {
            start,
            end: self.pos,
        }
    }

    pub fn next_token(&mut self) -> (Token, Span) {
        self.skip_whitespace_and_comments();

        let start = self.pos;
        let c = match self.advance() {
            Some(c) => c,
            None => {
                return (Token::EOF, Span { start, end: start });
            }
        };

        match c {
            '(' => return (Token::LParen, self.span(start)),
            ')' => return (Token::RParen, self.span(start)),
            '{' => return (Token::LBrace, self.span(start)),
            '}' => return (Token::RBrace, self.span(start)),
            '[' => return (Token::LBracket, self.span(start)),
            ']' => return (Token::RBracket, self.span(start)),
            ',' => return (Token::Comma, self.span(start)),
            ';' => return (Token::Semicolon, self.span(start)),
            ':' => {
                if self.peek() == Some(':') {
                    self.advance();
                    self.diags
                        .error("unexpected '::'", Some(self.span(start)));
                    return (
                        Token::Error("unexpected '::'".into()),
                        self.span(start),
                    );
                }
                return (Token::Colon, self.span(start));
            }
            '+' => return (Token::Plus, self.span(start)),
            '-' => {
                if self.peek() == Some('>') {
                    self.advance();
                    return (Token::Arrow, self.span(start));
                }
                return (Token::Minus, self.span(start));
            }
            '*' => return (Token::Star, self.span(start)),
            '/' => {
                if self.peek() == Some('/') {
                    self.skip_line_comment();
                    return self.next_token();
                }
                return (Token::Slash, self.span(start));
            }
            '=' => {
                if self.peek() == Some('=') {
                    self.advance();
                    return (Token::Eq, self.span(start));
                }
                return (Token::Assign, self.span(start));
            }
            '!' => {
                if self.peek() == Some('=') {
                    self.advance();
                    return (Token::Neq, self.span(start));
                }
                return (Token::Not, self.span(start));
            }
            '<' => {
                if self.peek() == Some('=') {
                    self.advance();
                    return (Token::Le, self.span(start));
                }
                if self.peek() == Some('<') {
                    self.advance();
                    self.diags
                        .error("unexpected '<<'", Some(self.span(start)));
                    return (
                        Token::Error("unexpected '<<'".into()),
                        self.span(start),
                    );
                }
                return (Token::Lt, self.span(start));
            }
            '>' => {
                if self.peek() == Some('=') {
                    self.advance();
                    return (Token::Ge, self.span(start));
                }
                if self.peek() == Some('>') {
                    self.advance();
                    self.diags
                        .error("unexpected '>>'", Some(self.span(start)));
                    return (
                        Token::Error("unexpected '>>'".into()),
                        self.span(start),
                    );
                }
                return (Token::Gt, self.span(start));
            }
            '&' => {
                if self.peek() == Some('&') {
                    self.advance();
                    return (Token::AndAnd, self.span(start));
                }
                self.diags
                    .error("unexpected '&'", Some(self.span(start)));
                return (
                    Token::Error("unexpected '&'".into()),
                    self.span(start),
                );
            }
            '|' => {
                if self.peek() == Some('|') {
                    self.advance();
                    return (Token::OrOr, self.span(start));
                }
                self.diags
                    .error("unexpected '|'", Some(self.span(start)));
                return (
                    Token::Error("unexpected '|'".into()),
                    self.span(start),
                );
            }
            _ => {}
        }

        if c.is_alphabetic() || c == '_' {
            return self.scan_ident(start);
        }

        if c.is_digit(10) {
            return self.scan_number(start);
        }

        self.diags
            .error(format!("unexpected character '{}'", c), Some(self.span(start)));
        (
            Token::Error(format!("unexpected character '{}'", c)),
            self.span(start),
        )
    }

    fn scan_ident(&mut self, start: usize) -> (Token, Span) {
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                self.advance();
            } else {
                break;
            }
        }
        let ident: String = self.chars[start..self.pos].iter().collect();
        if let Some(kw) = keyword_token(&ident) {
            (kw, self.span(start))
        } else {
            (Token::Ident(ident), self.span(start))
        }
    }

    fn scan_number(&mut self, start: usize) -> (Token, Span) {
        while let Some(c) = self.peek() {
            if c.is_digit(10) {
                self.advance();
            } else if c == '_' {
                self.advance();
            } else {
                break;
            }
        }
        let num_str: String = self.chars[start..self.pos]
            .iter()
            .filter(|c| **c != '_')
            .collect();
        match num_str.parse::<i64>() {
            Ok(val) => (Token::IntLit(val), self.span(start)),
            Err(_) => {
                self.diags
                    .error("integer literal overflow", Some(self.span(start)));
                (Token::Error("overflow".into()), self.span(start))
            }
        }
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            match self.peek() {
                Some(c) if c.is_whitespace() => {
                    self.advance();
                }
                Some('/') if self.peek_next() == Some('/') => {
                    self.skip_line_comment();
                }
                _ => break,
            }
        }
    }

    fn skip_line_comment(&mut self) {
        while let Some(c) = self.peek() {
            if c == '\n' {
                break;
            }
            self.advance();
        }
    }
}
