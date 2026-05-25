type prim_type = I32 | I64 | U32 | U64 | I8 | U8 | Bool | Void

type ast_type =
  | Prim of prim_type
  | Named of string
  | Ptr of ast_type
  | Array of ast_expr * ast_type

and ast_expr =
  | Int of int64
  | Bool of bool
  | Ident of string
  | Binary of binary_op * ast_expr * ast_expr
  | Unary of unary_op * ast_expr
  | Call of string * ast_expr list
  | Cast of ast_type * ast_expr
  | Assign of ast_expr * ast_expr

and binary_op = Add | Sub | Mul | Div | Eq | Neq | Lt | Gt | Le | Ge | And | Or
and unary_op = Neg | Not

type ast_param = { name: string; type_: ast_type }
type ast_block = { stmts: ast_stmt list }

and ast_stmt =
  | Return of ast_expr
  | VarDecl of { name: string; type_: ast_type; init: ast_expr option }
  | If of { cond: ast_expr; then_block: ast_block; else_branch: ast_stmt option }
  | While of { cond: ast_expr; body: ast_block }
  | Block of ast_block
  | Expr of ast_expr

type ast_func_decl = {
  name: string;
  return_type: ast_type;
  params: ast_param list;
  body: ast_block;
}

type ast_decl =
  | Func of ast_func_decl

type ast_program = { decls: ast_decl list }

let rec type_to_string = function
  | Prim I32 -> "i32" | Prim I64 -> "i64" | Prim U32 -> "u32" | Prim U64 -> "u64"
  | Prim I8 -> "i8" | Prim U8 -> "u8" | Prim Bool -> "bool" | Prim Void -> "void"
  | Named n -> n
  | Ptr t -> Printf.sprintf "@%s" (type_to_string t)
  | Array (e, t) -> Printf.sprintf "[%s]%s" (expr_to_string e) (type_to_string t)

and expr_to_string = function
  | Int n -> Int64.to_string n
  | Bool b -> if b then "true" else "false"
  | Ident s -> s
  | Binary (op, l, r) -> Printf.sprintf "(%s %s %s)" (expr_to_string l) (binop_to_string op) (expr_to_string r)
  | Unary (Neg, e) -> Printf.sprintf "(-%s)" (expr_to_string e)
  | Unary (Not, e) -> Printf.sprintf "(!%s)" (expr_to_string e)
  | Call (f, args) -> Printf.sprintf "%s(%s)" f (String.concat ", " (List.map expr_to_string args))
  | Cast (t, e) -> Printf.sprintf "cast(%s, %s)" (type_to_string t) (expr_to_string e)
  | Assign (l, r) -> Printf.sprintf "(%s = %s)" (expr_to_string l) (expr_to_string r)

and binop_to_string = function
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"
  | Eq -> "==" | Neq -> "!=" | Lt -> "<" | Gt -> ">"
  | Le -> "<=" | Ge -> ">=" | And -> "&&" | Or -> "||"
