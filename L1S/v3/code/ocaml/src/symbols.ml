type ir_type = I32 | I64 | U32 | U64 | I8 | U8 | Bool | Void | Ptr of ir_type

type symbol_kind = Func of ir_type list * ir_type | Var of ir_type
type symbol = { typ: ir_type; kind: symbol_kind }

type t = { mutable scopes: (string, symbol) Hashtbl.t list }

let create () = { scopes = [Hashtbl.create 16] }
let enter_scope st = st.scopes <- Hashtbl.create 16 :: st.scopes
let exit_scope st = st.scopes <- List.tl st.scopes
let insert st name sym = Hashtbl.add (List.hd st.scopes) name sym
let lookup st name =
  let rec search = function
    | [] -> None
    | h :: t -> match Hashtbl.find_opt h name with Some s -> Some s | None -> search t
  in
  search st.scopes

let ast_type_to_ir = function
  | Ast.Prim Ast.I32 -> I32 | Ast.Prim Ast.I64 -> I64
  | Ast.Prim Ast.U32 -> U32 | Ast.Prim Ast.U64 -> U64
  | Ast.Prim Ast.I8 -> I8 | Ast.Prim Ast.U8 -> U8
  | Ast.Prim Ast.Bool -> Bool | Ast.Prim Ast.Void -> Void
  | Ast.Ptr t -> Ptr (ast_type_to_ir t)
  | Ast.Named _ -> I32
  | Ast.Array _ -> Ptr I32

let ir_type_to_string = function
  | I32 -> "i32" | I64 -> "i64" | U32 -> "u32" | U64 -> "u64"
  | I8 -> "i8" | U8 -> "u8" | Bool -> "bool" | Void -> "void"
  | Ptr t -> Printf.sprintf "@%s" (ir_type_to_string t)
