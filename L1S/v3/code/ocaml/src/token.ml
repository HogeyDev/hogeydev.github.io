type token =
  | Func | Let | Return | If | Else | While
  | I32 | I64 | U32 | U64 | I8 | U8 | Bool | Void
  | Ident of string | NumLiteral of string
  | Plus | Minus | Star | Slash
  | Eq | EqEq | Bang | BangEq
  | Lt | Gt | LtEq | GtEq
  | AndAnd | PipePipe
  | OpenParen | CloseParen
  | OpenBrace | CloseBrace
  | OpenBracket | CloseBracket
  | Colon | Comma | Semicolon | At
  | Eof

type spanned_token = token * Span.span

let keyword_token = function
  | "func" -> Some Func | "let" -> Some Let | "return" -> Some Return
  | "if" -> Some If | "else" -> Some Else | "while" -> Some While
  | "i32" -> Some I32 | "i64" -> Some I64 | "u32" -> Some U32 | "u64" -> Some U64
  | "i8" -> Some I8 | "u8" -> Some U8 | "bool" -> Some Bool | "void" -> Some Void
  | _ -> None

let token_to_string = function
  | Func -> "func" | Let -> "let" | Return -> "return"
  | If -> "if" | Else -> "else" | While -> "while"
  | I32 -> "i32" | I64 -> "i64" | U32 -> "u32" | U64 -> "u64"
  | I8 -> "i8" | U8 -> "u8" | Bool -> "bool" | Void -> "void"
  | Ident s -> Printf.sprintf "Ident(%s)" s
  | NumLiteral s -> Printf.sprintf "Num(%s)" s
  | Plus -> "+" | Minus -> "-" | Star -> "*" | Slash -> "/"
  | Eq -> "=" | EqEq -> "==" | Bang -> "!" | BangEq -> "!="
  | Lt -> "<" | Gt -> ">" | LtEq -> "<=" | GtEq -> ">="
  | AndAnd -> "&&" | PipePipe -> "||"
  | OpenParen -> "(" | CloseParen -> ")"
  | OpenBrace -> "{" | CloseBrace -> "}"
  | OpenBracket -> "[" | CloseBracket -> "]"
  | Colon -> ":" | Comma -> "," | Semicolon -> ";"
  | At -> "@" | Eof -> "EOF"
