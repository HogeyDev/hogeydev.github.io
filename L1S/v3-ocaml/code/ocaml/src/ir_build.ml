open Ast
open Ir
open Span

module StrMap = Map.Make(String)

type block_state = {
  mutable preds: string list;
  mutable sealed: bool;
  mutable instrs: ir_instr list;
  mutable terminator: ir_terminator;
}

type builder = {
  diags: Diag.t;
  mutable module_funcs: ir_function list;
  mutable current_block: string;
  mutable block_counter: int;
  mutable vreg_counter: int;
  mutable var_slot_counter: int;
  mutable var_slots: int StrMap.t;
  mutable blocks: block_state StrMap.t;
}

let create diags = {
  diags;
  module_funcs = [];
  current_block = "";
  block_counter = 0;
  vreg_counter = 0;
  var_slot_counter = 0;
  var_slots = StrMap.empty;
  blocks = StrMap.empty;
}

let new_vreg b =
  let v = b.vreg_counter in
  b.vreg_counter <- b.vreg_counter + 1;
  v

let gen_label b prefix =
  let l = Printf.sprintf "%s_%d" prefix b.block_counter in
  b.block_counter <- b.block_counter + 1;
  l

let ast_type_to_ir = function
  | TPrim TI32 -> I32
  | TPrim TBool -> Bool
  | TPrim TVoid -> Void
  | TPtr _ -> Ptr (Box I32)

let add_block b label =
  if not (StrMap.mem label b.blocks) then
    b.blocks <- StrMap.add label {
      preds = []; sealed = false;
      instrs = []; terminator = Unreachable;
    } b.blocks

let set_current_block b label =
  b.current_block <- label

let add_pred b block pred =
  let bs = StrMap.find block b.blocks in
  if not (List.mem pred bs.preds) then
    bs.preds <- pred :: bs.preds

let emit_instr b instr =
  let bs = StrMap.find b.current_block b.blocks in
  bs.instrs <- instr :: bs.instrs

let set_terminator b block term =
  let bs = StrMap.find block b.blocks in
  bs.terminator <- term

let block_has_terminator b block =
  try
    let bs = StrMap.find block b.blocks in
    match bs.terminator with
    | Unreachable -> false
    | _ -> true
  with Not_found -> false

let alloc_var_slot b =
  let s = b.var_slot_counter in
  b.var_slot_counter <- b.var_slot_counter + 1;
  s

let write_var_slot b name slot =
  b.var_slots <- StrMap.add name slot b.var_slots

let lookup_var_slot b name =
  StrMap.find_opt name b.var_slots

let lookup_var_slot_or_err b name =
  match lookup_var_slot b name with
  | Some s -> s
  | None ->
    let s = alloc_var_slot b in
    Diag.error b.diags (Printf.sprintf "undefined variable '%s'" name);
    s

let rec seal_block b label =
  match StrMap.find_opt label b.blocks with
  | Some bs' ->
    bs'.sealed <- true;
    let unsealed = StrMap.filter (fun _ s -> not s.sealed) b.blocks in
    StrMap.iter (fun l _ ->
      let ubs = StrMap.find l b.blocks in
      let all_preds_sealed = List.for_all (fun p ->
        match StrMap.find_opt p b.blocks with
        | Some ps -> ps.sealed | None -> false
      ) ubs.preds in
      if ubs.preds <> [] && all_preds_sealed then
        seal_block b l
    ) unsealed
  | None -> ()

let rec process_expr b expr =
  match fst expr with
  | EInt n ->
    let v = new_vreg b in
    emit_instr b (Const (v, n, I32));
    v
  | EBool bv ->
    let v = new_vreg b in
    emit_instr b (Const (v, if bv then 1L else 0L, Bool));
    v
  | EIdent name ->
    let dest = new_vreg b in
    let slot = lookup_var_slot_or_err b name in
    emit_instr b (LoadStack (dest, slot, I32));
    dest
  | EBinary (op, lhs, rhs) ->
    let lv = process_expr b lhs in
    let rv = process_expr b rhs in
    let dest = new_vreg b in
    let instr = match op with
      | Add -> Add (dest, lv, rv) | Sub -> Sub (dest, lv, rv)
      | Mul -> Mul (dest, lv, rv) | Div -> Div (dest, lv, rv)
      | Eq -> Eq (dest, lv, rv) | Neq -> Neq (dest, lv, rv)
      | Lt -> Lt (dest, lv, rv) | Gt -> Gt (dest, lv, rv)
      | Le -> Le (dest, lv, rv) | Ge -> Ge (dest, lv, rv)
      | And -> And (dest, lv, rv) | Or -> Or (dest, lv, rv)
    in
    emit_instr b instr;
    dest
  | EUnary (op, e) ->
    let v = process_expr b e in
    let dest = new_vreg b in
    let instr = match op with
      | Neg -> Neg (dest, v) | Not -> Not (dest, v)
    in
    emit_instr b instr;
    dest
  | ECall (name, args) ->
    let arg_vregs = List.map (fun a -> process_expr b a) args in
    let dest = new_vreg b in
    emit_instr b (Call (dest, name, arg_vregs, I32));
    dest
  | ECast (_, e) ->
    process_expr b e

let rec process_stmt b stmt =
  if not (block_has_terminator b b.current_block) then
    match fst stmt with
    | SReturn expr_opt ->
      let vreg = Option.map (fun e -> process_expr b e) expr_opt in
      set_terminator b b.current_block (Ret vreg)
    | SVarDecl (name, typ, init) ->
      let ir_type = ast_type_to_ir typ in
      let slot = alloc_var_slot b in
      (match init with
       | Some init_expr ->
         let rhs = process_expr b init_expr in
         emit_instr b (StoreStack (rhs, slot, ir_type))
       | None -> ());
      write_var_slot b name slot
    | SIf (cond, then_body, else_body) ->
      process_if b cond then_body else_body
    | SWhile (cond, body) ->
      process_while b cond body
    | SBlock stmts ->
      List.iter (fun s -> process_stmt b s) stmts
    | SExpr expr ->
      ignore (process_expr b expr)
    | SAssign (name, expr) ->
      let rhs = process_expr b expr in
      (match lookup_var_slot b name with
       | Some slot -> emit_instr b (StoreStack (rhs, slot, I32))
       | None ->
         let slot = alloc_var_slot b in
         emit_instr b (StoreStack (rhs, slot, I32));
         write_var_slot b name slot)

and process_if b cond then_body else_body =
  let cond_vreg = process_expr b cond in
  let then_label = gen_label b "then" in
  let else_label = gen_label b "else" in
  let merge_label = gen_label b "merge" in
  let cb = b.current_block in
  add_pred b then_label cb;
  add_pred b else_label cb;
  set_terminator b cb (BrCond (cond_vreg, then_label, else_label));
  add_block b then_label;
  set_current_block b then_label;
  seal_block b then_label;
  List.iter (fun s -> process_stmt b s) then_body;
  let cb2 = b.current_block in
  if not (block_has_terminator b cb2) then begin
    add_pred b merge_label cb2;
    set_terminator b cb2 (Br merge_label)
  end;
  add_block b else_label;
  set_current_block b else_label;
  seal_block b else_label;
  (match else_body with
   | Some eb -> List.iter (fun s -> process_stmt b s) eb
   | None -> ());
  let cb3 = b.current_block in
  if not (block_has_terminator b cb3) then begin
    add_pred b merge_label cb3;
    set_terminator b cb3 (Br merge_label)
  end;
  add_block b merge_label;
  set_current_block b merge_label;
  seal_block b merge_label

and process_while b cond body =
  let header_label = gen_label b "header" in
  let body_label = gen_label b "body" in
  let exit_label = gen_label b "exit" in
  let cb = b.current_block in
  add_pred b header_label cb;
  set_terminator b cb (Br header_label);
  add_block b header_label;
  set_current_block b header_label;
  let cond_vreg = process_expr b cond in
  let cb2 = b.current_block in
  add_pred b body_label cb2;
  add_pred b exit_label cb2;
  set_terminator b cb2 (BrCond (cond_vreg, body_label, exit_label));
  add_block b body_label;
  set_current_block b body_label;
  seal_block b body_label;
  List.iter (fun s -> process_stmt b s) body;
  let cb3 = b.current_block in
  if not (block_has_terminator b cb3) then begin
    add_pred b header_label cb3;
    set_terminator b cb3 (Br header_label)
  end;
  seal_block b header_label;
  add_block b exit_label;
  set_current_block b exit_label;
  seal_block b exit_label

let dfs_order b label order visited =
  if List.mem label !visited then ()
  else begin
    visited := label :: !visited;
    order := !order @ [label];
    match StrMap.find_opt label b.blocks with
    | Some bs ->
      (match bs.terminator with
       | Br t -> dfs_order b t order visited
       | BrCond (_, t, f) -> dfs_order b t order visited; dfs_order b f order visited
       | _ -> ())
    | None -> ()
  end

let build_function b func =
  let ret_type = ast_type_to_ir func.f_return_type in
  let func_name = func.f_name in
  b.vreg_counter <- 0;
  b.block_counter <- 0;
  b.var_slot_counter <- 0;
  b.var_slots <- StrMap.empty;
  b.blocks <- StrMap.empty;

  let entry_label = gen_label b "entry" in
  add_block b entry_label;
  set_current_block b entry_label;

  let ptypes = List.map (fun p -> ast_type_to_ir p.p_type) func.f_params in
  let params = List.mapi (fun i p ->
    let vreg = new_vreg b in
    { p_name = p.p_name; p_vreg = vreg; p_type = List.nth ptypes i }
  ) func.f_params in

  List.iter (fun p ->
    let slot = alloc_var_slot b in
    emit_instr b (StoreStack (p.p_vreg, slot, p.p_type));
    write_var_slot b p.p_name slot
  ) params;

  seal_block b entry_label;

  List.iter (fun stmt ->
    process_stmt b stmt
  ) func.f_body;

  let last_block = b.current_block in
  let has_term = block_has_terminator b last_block in
  if not has_term then begin
    let is_unreachable = try
      let bs = StrMap.find last_block b.blocks in
      bs.preds = []
    with Not_found -> true
    in
    if not is_unreachable then
      set_terminator b last_block (Ret None)
  end;

  let order = ref [] in
  let visited = ref [] in
  dfs_order b entry_label order visited;

  let blocks = List.filter_map (fun label ->
    match StrMap.find_opt label b.blocks with
    | Some bs -> Some {
        label; instrs = List.rev bs.instrs; terminator = bs.terminator
      }
    | None -> None
  ) !order in

  let irf = {
    f_name = func_name;
    f_params = params;
    f_return_type = ret_type;
    f_blocks = blocks;
    f_num_vregs = b.vreg_counter;
    f_num_var_slots = b.var_slot_counter;
  } in
  b.module_funcs <- b.module_funcs @ [irf]

let build b prog =
  List.iter (fun decl ->
    match decl with
    | AFunc f -> build_function b f
  ) prog.decls
