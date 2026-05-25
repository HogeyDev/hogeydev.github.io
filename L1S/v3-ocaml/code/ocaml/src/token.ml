type token_kind =
  | TInt of int64
  | TIdent of string
  | TFunc
  | TLet
  | TMut
  | TIf
  | TElse
  | TWhile
  | TReturn
  | TTrue
  | TFalse
  | TI32
  | TBool
  | TVoid
  | TColon
  | TSemicolon
  | TComma
  | TLParen
  | TRParen
  | TLBrace
  | TRBrace
  | TPlus
  | TMinus
  | TStar
  | TSlash
  | TEquals
  | TEqEq
  | TNeq
  | TLt
  | TGt
  | TLe
  | TGe
  | TAnd
  | TOr
  | TNot
  | TArrow
  | TEOF

type token = { kind: token_kind; lexeme: string; span: Span.span }

let kind_to_string = function
  | TInt n -> Printf.sprintf "int(%Ld)" n
  | TIdent s -> Printf.sprintf "ident(%s)" s
  | TFunc -> "func"
  | TLet -> "let"
  | TMut -> "mut"
  | TIf -> "if"
  | TElse -> "else"
  | TWhile -> "while"
  | TReturn -> "return"
  | TTrue -> "true"
  | TFalse -> "false"
  | TI32 -> "i32"
  | TBool -> "bool"
  | TVoid -> "void"
  | TColon -> ":"
  | TSemicolon -> ";"
  | TComma -> ","
  | TLParen -> "("
  | TRParen -> ")"
  | TLBrace -> "{"
  | TRBrace -> "}"
  | TPlus -> "+"
  | TMinus -> "-"
  | TStar -> "*"
  | TSlash -> "/"
  | TEquals -> "="
  | TEqEq -> "=="
  | TNeq -> "!="
  | TLt -> "<"
  | TGt -> ">"
  | TLe -> "<="
  | TGe -> ">="
  | TAnd -> "&&"
  | TOr -> "||"
  | TNot -> "!"
  | TArrow -> "->"
  | TEOF -> "EOF"
