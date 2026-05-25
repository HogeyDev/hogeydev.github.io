type vreg = int

type ir_type = I32 | I64 | U32 | U64 | I8 | U8 | Bool | Void | Ptr of ir_type

type ir_instr =
  | Phi of vreg * (vreg * string) list
  | Const of vreg * int64 * ir_type
  | Add of vreg * vreg * vreg
  | Sub of vreg * vreg * vreg
  | Mul of vreg * vreg * vreg
  | Div of vreg * vreg * vreg
  | Eq of vreg * vreg * vreg
  | Neq of vreg * vreg * vreg
  | Lt of vreg * vreg * vreg
  | Gt of vreg * vreg * vreg
  | Le of vreg * vreg * vreg
  | Ge of vreg * vreg * vreg
  | Not of vreg * vreg
  | Neg of vreg * vreg
  | And of vreg * vreg * vreg
  | Or of vreg * vreg * vreg
  | Load of vreg * vreg * ir_type
  | Store of vreg * vreg * ir_type
  | LoadStack of vreg * int * ir_type
  | StoreStack of vreg * int * ir_type
  | Alloc of vreg * ir_type
  | Call of vreg * string * vreg list * ir_type
  | Copy of vreg * vreg

type ir_terminator =
  | Ret of vreg option
  | Br of string
  | BrCond of vreg * string * string
  | Unreachable

type ir_block = {
  label: string;
  instrs: ir_instr list;
  terminator: ir_terminator;
}

type ir_param = {
  p_name: string;
  p_vreg: vreg;
  p_type: ir_type;
}

type ir_function = {
  f_name: string;
  f_params: ir_param list;
  f_return_type: ir_type;
  f_blocks: ir_block list;
  f_num_vregs: int;
  f_num_var_slots: int;
}

type ir_module = { funcs: ir_function list }

let dest_of_instr = function
  | Phi (d, _) | Const (d, _, _) | Add (d, _, _) | Sub (d, _, _)
  | Mul (d, _, _) | Div (d, _, _) | Eq (d, _, _) | Neq (d, _, _)
  | Lt (d, _, _) | Gt (d, _, _) | Le (d, _, _) | Ge (d, _, _)
  | Not (d, _) | Neg (d, _) | And (d, _, _) | Or (d, _, _)
  | Load (d, _, _) | LoadStack (d, _, _) | Alloc (d, _)
  | Call (d, _, _, _) | Copy (d, _) -> Some d
  | Store _ | StoreStack _ -> None

let uses_of_instr = function
  | Phi (_, ops) -> List.map fst ops
  | Const _ -> []
  | Add (_, a, b) | Sub (_, a, b) | Mul (_, a, b) | Div (_, a, b)
  | Eq (_, a, b) | Neq (_, a, b) | Lt (_, a, b) | Gt (_, a, b)
  | Le (_, a, b) | Ge (_, a, b) | And (_, a, b) | Or (_, a, b) -> [a; b]
  | Not (_, a) | Neg (_, a) | Copy (_, a) -> [a]
  | Load (_, addr, _) -> [addr]
  | LoadStack _ -> []
  | Store (v, a, _) -> [v; a]
  | StoreStack (v, _, _) -> [v]
  | Alloc _ -> []
  | Call (_, _, args, _) -> args

let is_side_effect = function
  | Store _ | StoreStack _ | Call _ -> true
  | _ -> false
