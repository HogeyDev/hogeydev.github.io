use crate::span::Span;

pub type SpannedToken = (Token, Span);

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Func, Let, Return, If, Else, While, For, Asm, Import, Static, Spec, Pub,
    True, False,
    I32, I64, U32, U64, I8, U8, Bool, Char, Void, Isize, Usize,
    Ident(String), NumLiteral(String), StrLiteral(String),
    Plus, Minus, Star, Slash,
    Eq, EqEq, Bang, BangEq, Lt, Gt, LtEq, GtEq, AndAnd, PipePipe,
    OpenParen, CloseParen, OpenBrace, CloseBrace, OpenBracket, CloseBracket,
    Colon, Comma, Semicolon, Dot, Arrow, At,
    Eof,
}

pub fn keyword_token(s: &str) -> Option<Token> {
    match s {
        "func" => Some(Token::Func),
        "let" => Some(Token::Let),
        "return" => Some(Token::Return),
        "if" => Some(Token::If),
        "else" => Some(Token::Else),
        "while" => Some(Token::While),
        "for" => Some(Token::For),
        "asm" => Some(Token::Asm),
        "import" => Some(Token::Import),
        "static" => Some(Token::Static),
        "spec" => Some(Token::Spec),
        "pub" => Some(Token::Pub),
        "true" => Some(Token::True),
        "false" => Some(Token::False),
        "i32" => Some(Token::I32),
        "i64" => Some(Token::I64),
        "u32" => Some(Token::U32),
        "u64" => Some(Token::U64),
        "i8" => Some(Token::I8),
        "u8" => Some(Token::U8),
        "bool" => Some(Token::Bool),
        "char" => Some(Token::Char),
        "void" => Some(Token::Void),
        "isize" => Some(Token::Isize),
        "usize" => Some(Token::Usize),
        _ => None,
    }
}
