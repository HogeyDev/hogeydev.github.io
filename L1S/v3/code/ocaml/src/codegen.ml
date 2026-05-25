let emit_line buf fmt = Printf.ksprintf (fun s -> Buffer.add_string buf (s ^ "\n")) fmt

let get_slot func vreg =
  try Hashtbl.find func.Ir.slot_map vreg
  with Not_found -> 0

let slot_addr offset = Printf.sprintf "[rbp-%d]" offset

let load_vreg buf func vreg =
  let offset = get_slot func vreg in
  if offset = 0 && false then ()  (* would be in a register *)
  else emit_line buf "\tmov rax, %s" (slot_addr offset)

let store_rax buf func vreg =
  let offset = get_slot func vreg in
  emit_line buf "\tmov %s, rax" (slot_addr offset)

let gen_prologue buf func =
  emit_line buf "\tpush rbp";
  emit_line buf "\tmov rbp, rsp";
  if func.Ir.frame_size > 0 then
    emit_line buf "\tsub rsp, %d" func.Ir.frame_size

let gen_epilogue buf func =
  emit_line buf "\tmov rsp, rbp";
  emit_line buf "\tpop rbp";
  emit_line buf "\tret"

let gen_instr buf func instr =
  match instr with
  | Ir.Const (v, n, _) ->
    emit_line buf "\tmov rax, %Ld" n;
    store_rax buf func v
  | Ir.Add (v, a, b) ->
    load_vreg buf func a;
    emit_line buf "\tpush rax";
    load_vreg buf func b;
    emit_line buf "\tmov rcx, rax";
    emit_line buf "\tpop rax";
    emit_line buf "\tadd rax, rcx";
    store_rax buf func v
  | Ir.Sub (v, a, b) ->
    load_vreg buf func a;
    emit_line buf "\tpush rax";
    load_vreg buf func b;
    emit_line buf "\tmov rcx, rax";
    emit_line buf "\tpop rax";
    emit_line buf "\tsub rax, rcx";
    store_rax buf func v
  | Ir.Mul (v, a, b) ->
    load_vreg buf func a;
    emit_line buf "\tpush rax";
    load_vreg buf func b;
    emit_line buf "\tmov rcx, rax";
    emit_line buf "\tpop rax";
    emit_line buf "\timul rax, rcx";
    store_rax buf func v
  | Ir.Div (v, a, b) ->
    load_vreg buf func a;
    emit_line buf "\tpush rax";
    load_vreg buf func b;
    emit_line buf "\tmov rcx, rax";
    emit_line buf "\tpop rax";
    emit_line buf "\txor rdx, rdx";
    emit_line buf "\tidiv rcx";
    store_rax buf func v
  | Ir.Eq (v, a, b) ->
    load_vreg buf func a;
    emit_line buf "\tpush rax";
    load_vreg buf func b;
    emit_line buf "\tmov rcx, rax";
    emit_line buf "\tpop rax";
    emit_line buf "\tcmp rax, rcx";
    emit_line buf "\tsete al";
    emit_line buf "\tmovzx rax, al";
    store_rax buf func v
  | Ir.Lt (v, a, b) ->
    load_vreg buf func a;
    emit_line buf "\tpush rax";
    load_vreg buf func b;
    emit_line buf "\tmov rcx, rax";
    emit_line buf "\tpop rax";
    emit_line buf "\tcmp rax, rcx";
    emit_line buf "\tsetl al";
    emit_line buf "\tmovzx rax, al";
    store_rax buf func v
  | Ir.And (v, a, b) ->
    load_vreg buf func a;
    emit_line buf "\tpush rax";
    load_vreg buf func b;
    emit_line buf "\tmov rcx, rax";
    emit_line buf "\tpop rax";
    emit_line buf "\tand rax, rcx";
    store_rax buf func v
  | Ir.Or (v, a, b) ->
    load_vreg buf func a;
    emit_line buf "\tpush rax";
    load_vreg buf func b;
    emit_line buf "\tmov rcx, rax";
    emit_line buf "\tpop rax";
    emit_line buf "\tor rax, rcx";
    store_rax buf func v
  | Ir.Not (v, a) ->
    load_vreg buf func a;
    emit_line buf "\tnot rax";
    store_rax buf func v
  | Ir.Neg (v, a) ->
    load_vreg buf func a;
    emit_line buf "\tneg rax";
    store_rax buf func v
  | Ir.Load (v, src, _) ->
    load_vreg buf func src;
    emit_line buf "\tmov rax, [rax]";
    store_rax buf func v
  | Ir.Store (dst, src, _) ->
    load_vreg buf func dst;
    emit_line buf "\tpush rax";
    load_vreg buf func src;
    emit_line buf "\tpop rcx";
    emit_line buf "\tmov [rcx], rax"
  | Ir.Alloc _ -> ()  (* handled by frame allocation *)
  | Ir.Call (v, name, args, _) ->
    let arg_regs = [| "rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9" |] in
    List.iteri (fun i a ->
      if i < 6 then begin
        load_vreg buf func a;
        emit_line buf "\tmov %s, rax" arg_regs.(i)
      end
    ) args;
    if List.length args > 6 then
      (* push extra args to stack in reverse *)
      let extra = List.rev (List.drop 6 args) in
      List.iter (fun a ->
        load_vreg buf func a;
        emit_line buf "\tpush rax"
      ) extra;
    emit_line buf "\tcall %s" name;
    store_rax buf func v

let gen_terminator buf func = function
  | Ir.Ret v ->
    load_vreg buf func v;
    gen_epilogue buf func
  | Ir.Br label ->
    emit_line buf "\tjmp %s" label
  | Ir.BrCond (v, t_label, f_label) ->
    load_vreg buf func v;
    emit_line buf "\ttest rax, rax";
    emit_line buf "\tjnz %s" t_label;
    emit_line buf "\tjmp %s" f_label

let gen_func buf func =
  emit_line buf "";
  emit_line buf "global %s" func.Ir.name;
  emit_line buf "%s:" func.Ir.name;
  gen_prologue buf func;
  List.iter (fun block ->
    emit_line buf "%s:" block.Ir.label;
    List.iter (fun instr -> gen_instr buf func instr) (List.rev block.Ir.instrs);
    gen_terminator buf func block.Ir.terminator
  ) func.Ir.blocks

let generate_module modl =
  let buf = Buffer.create 4096 in
  emit_line buf "default rel";
  emit_line buf "section .text";
  List.iter (fun func -> gen_func buf func) modl.Ir.funcs;
  Buffer.contents buf
