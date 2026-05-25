open Span

type prim_type = TI32 | TBool | TVoid

type ast_type =
  | TPrim of prim_type
  | TPtr of ast_type

type binary_op = Add | Sub | Mul | Div | Eq | Neq | Lt | Gt | Le | Ge | And | Or
type unary_op = Neg | Not

type expr_kind =
  | EInt of int64
  | EBool of bool
  | EIdent of string
  | EBinary of binary_op * expr * expr
  | EUnary of unary_op * expr
  | ECall of string * expr list
  | ECast of ast_type * expr

and expr = expr_kind * span

type stmt_kind =
  | SReturn of expr option
  | SVarDecl of string * ast_type * expr option
  | SIf of expr * stmt list * stmt list option
  | SWhile of expr * stmt list
  | SBlock of stmt list
  | SExpr of expr
  | SAssign of string * expr

and stmt = stmt_kind * span

type ast_param = { p_name: string; p_name_span: span; p_type: ast_type; p_type_span: span }

type ast_func_decl = {
  f_name: string;
  f_name_span: span;
  f_params: ast_param list;
  f_return_type: ast_type;
  f_return_span: span;
  f_body: stmt list;
  f_span: span;
}

type ast_decl =
  | AFunc of ast_func_decl

type ast_program = { decls: ast_decl list }
