type ir_type = I32 | I64 | U32 | U64 | I8 | U8 | Bool | Void | Ptr of ir_type

type vreg = int

type ir_instr =
  | Const of vreg * int64 * ir_type
  | Add of vreg * vreg * vreg
  | Sub of vreg * vreg * vreg
  | Mul of vreg * vreg * vreg
  | Div of vreg * vreg * vreg
  | Eq of vreg * vreg * vreg
  | Lt of vreg * vreg * vreg
  | And of vreg * vreg * vreg
  | Or of vreg * vreg * vreg
  | Not of vreg * vreg
  | Neg of vreg * vreg
  | Load of vreg * vreg * ir_type
  | Store of vreg * vreg * ir_type
  | Call of vreg * string * vreg list * ir_type
  | Alloc of vreg * ir_type

type ir_terminator =
  | Ret of vreg
  | Br of string
  | BrCond of vreg * string * string

type ir_block = { label: string; instrs: ir_instr list; terminator: ir_terminator }

type ir_function = {
  name: string;
  params: ir_type list;
  return_type: ir_type;
  blocks: ir_block list;
  mutable num_vregs: int;
  slot_map: (int, int) Hashtbl.t;
  frame_size: int;
}

type ir_module = { funcs: ir_function list; globals: (string * ir_type) list }

let fresh_vreg func =
  let v = func.num_vregs in
  func.num_vregs <- func.num_vregs + 1;
  v

let make_block label = { label; instrs = []; terminator = Ret 0 }

let make_function name ret_type =
  { name; params = []; return_type = ret_type; blocks = []; num_vregs = 0;
    slot_map = Hashtbl.create 16; frame_size = 0 }

let ir_type_to_string = function
  | I32 -> "i32" | I64 -> "i64" | U32 -> "u32" | U64 -> "u64"
  | I8 -> "i8" | U8 -> "u8" | Bool -> "bool" | Void -> "void"
  | Ptr t -> Printf.sprintf "@%s" (ir_type_to_string t)
