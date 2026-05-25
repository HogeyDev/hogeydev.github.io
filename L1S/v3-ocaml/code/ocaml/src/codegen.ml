open Ir
open Regalloc

module VRegMap = Map.Make(Int)

let starts_with_prefix s prefix =
  let slen = String.length s in
  let plen = String.length prefix in
  plen <= slen && String.sub s 0 plen = prefix

type codegen = {
  mutable output: Buffer.t;
  mutable current_var_slots: int;
}

let create () = { output = Buffer.create 4096; current_var_slots = 0 }

let emit cg line =
  Buffer.add_string cg.output line;
  Buffer.add_char cg.output '\n'

let phys_name = Regalloc.phys_name

let vreg_loc cg vreg alloc =
  match VRegMap.find_opt vreg alloc.reg_map with
  | Some reg -> phys_name reg
  | None ->
    (match VRegMap.find_opt vreg alloc.spill_slots with
     | Some slot ->
       let offset = cg.current_var_slots * 8 + (slot + 1) * 8 in
       Printf.sprintf "[rbp-%d]" offset
     | None -> "0")

let get_param_reg vreg params =
  let rec find i = function
    | [] -> None
    | p :: rest ->
      if p.p_vreg = vreg && i < 6 then Some (arg_reg_name i)
      else find (i + 1) rest
  in
  find 0 params

let collect_phi_resolves func alloc =
  (* No phi support needed for this simple SSA *)
  []

let rec emit_instr cg instr alloc =
  match instr with
  | Const (vreg, n, _) ->
    let loc = vreg_loc cg vreg alloc in
    if starts_with_prefix "[" loc then begin
      emit cg (Printf.sprintf "    mov rax, %Ld" n);
      emit cg (Printf.sprintf "    mov %s, rax" loc)
    end else
      emit cg (Printf.sprintf "    mov %s, %Ld" loc n)

  | Add (vreg, a, b) -> binop cg "add" vreg a b alloc
  | Sub (vreg, a, b) -> binop cg "sub" vreg a b alloc
  | Mul (vreg, a, b) -> binop cg "imul" vreg a b alloc
  | Div (vreg, a, b) -> emit_div cg vreg a b alloc

  | Eq (vreg, a, b) -> setcc cg "sete" vreg a b alloc
  | Neq (vreg, a, b) -> setcc cg "setne" vreg a b alloc
  | Lt (vreg, a, b) -> setcc cg "setl" vreg a b alloc
  | Gt (vreg, a, b) -> setcc cg "setg" vreg a b alloc
  | Le (vreg, a, b) -> setcc cg "setle" vreg a b alloc
  | Ge (vreg, a, b) -> setcc cg "setge" vreg a b alloc

  | Not (vreg, s) ->
    let dst = vreg_loc cg vreg alloc in
    let src = vreg_loc cg s alloc in
    if starts_with_prefix "[" dst then begin
      emit cg (Printf.sprintf "    mov rax, %s" src);
      emit cg "    xor rax, 1";
      emit cg (Printf.sprintf "    mov %s, rax" dst)
    end else begin
      if dst <> src then emit cg (Printf.sprintf "    mov %s, %s" dst src);
      emit cg (Printf.sprintf "    xor %s, 1" dst)
    end

  | Neg (vreg, s) ->
    let dst = vreg_loc cg vreg alloc in
    let src = vreg_loc cg s alloc in
    if starts_with_prefix "[" dst then begin
      emit cg (Printf.sprintf "    mov rax, %s" src);
      emit cg "    neg rax";
      emit cg (Printf.sprintf "    mov %s, rax" dst)
    end else begin
      if dst <> src then emit cg (Printf.sprintf "    mov %s, %s" dst src);
      emit cg (Printf.sprintf "    neg %s" dst)
    end

  | And (vreg, a, b) -> binop cg "and" vreg a b alloc
  | Or (vreg, a, b) -> binop cg "or" vreg a b alloc

  | Load (vreg, addr, _) ->
    let dst = vreg_loc cg vreg alloc in
    let src = vreg_loc cg addr alloc in
    if starts_with_prefix "[" dst then begin
      emit cg (Printf.sprintf "    mov rax, [%s]" src);
      emit cg (Printf.sprintf "    mov %s, rax" dst)
    end else
      emit cg (Printf.sprintf "    mov %s, [%s]" dst src)

  | Store (val_, addr, _) ->
    let vloc = vreg_loc cg val_ alloc in
    let aloc = vreg_loc cg addr alloc in
    emit cg (Printf.sprintf "    mov rax, %s" vloc);
    emit cg (Printf.sprintf "    mov [%s], rax" aloc)

  | Alloc _ -> ()

  | LoadStack (vreg, slot, _) ->
    let dst = vreg_loc cg vreg alloc in
    let offset = (slot + 1) * 8 in
    if starts_with_prefix "[" dst then begin
      emit cg (Printf.sprintf "    mov rax, [rbp-%d]" offset);
      emit cg (Printf.sprintf "    mov %s, rax" dst)
    end else
      emit cg (Printf.sprintf "    mov %s, [rbp-%d]" dst offset)

  | StoreStack (val_, slot, _) ->
    let vloc = vreg_loc cg val_ alloc in
    let offset = (slot + 1) * 8 in
    emit cg (Printf.sprintf "    mov rax, %s" vloc);
    emit cg (Printf.sprintf "    mov [rbp-%d], rax" offset)

  | Call (vreg, name, args, _) ->
    List.iteri (fun i arg ->
      if i < 6 then begin
        let arg_reg = arg_reg_name i in
        let loc = vreg_loc cg arg alloc in
        emit cg (Printf.sprintf "    mov %s, %s" arg_reg loc)
      end else begin
        let loc = vreg_loc cg arg alloc in
        emit cg (Printf.sprintf "    push %s" loc)
      end
    ) args;
    let callee = if name = "main" then "main_func" else name in
    emit cg (Printf.sprintf "    call %s" callee);
    let dst = vreg_loc cg vreg alloc in
    if starts_with_prefix "[" dst then
      emit cg (Printf.sprintf "    mov %s, rax" dst)
    else if dst <> "rax" then
      emit cg (Printf.sprintf "    mov %s, rax" dst)

  | Copy (vreg, s) ->
    let dst = vreg_loc cg vreg alloc in
    let src = vreg_loc cg s alloc in
    if dst <> src then begin
      if starts_with_prefix "[" dst && starts_with_prefix "[" src then begin
        emit cg (Printf.sprintf "    mov rax, %s" src);
        emit cg (Printf.sprintf "    mov %s, rax" dst)
      end else
        emit cg (Printf.sprintf "    mov %s, %s" dst src)
    end

  | Phi _ -> ()

and binop cg asm_op dest a b alloc =
  let dst = vreg_loc cg dest alloc in
  let aloc = vreg_loc cg a alloc in
  let bloc = vreg_loc cg b alloc in
  if starts_with_prefix "[" dst then begin
    emit cg (Printf.sprintf "    mov rax, %s" aloc);
    emit cg (Printf.sprintf "    %s rax, %s" asm_op bloc);
    emit cg (Printf.sprintf "    mov %s, rax" dst)
  end else begin
    if dst <> aloc then emit cg (Printf.sprintf "    mov %s, %s" dst aloc);
    emit cg (Printf.sprintf "    %s %s, %s" asm_op dst bloc)
  end

and setcc cg cc dest a b alloc =
  let dst = vreg_loc cg dest alloc in
  let aloc = vreg_loc cg a alloc in
  let bloc = vreg_loc cg b alloc in
  if starts_with_prefix "[" dst then begin
    emit cg (Printf.sprintf "    mov rax, %s" aloc);
    emit cg (Printf.sprintf "    cmp rax, %s" bloc);
    emit cg (Printf.sprintf "    %s al" cc);
    emit cg "    movzx rax, al";
    emit cg (Printf.sprintf "    mov %s, rax" dst)
  end else begin
    emit cg (Printf.sprintf "    mov %s, %s" dst aloc);
    emit cg (Printf.sprintf "    cmp %s, %s" dst bloc);
    emit cg (Printf.sprintf "    %s al" cc);
    emit cg (Printf.sprintf "    movzx %s, al" dst)
  end

and emit_div cg dest a b alloc =
  let dst = vreg_loc cg dest alloc in
  let aloc = vreg_loc cg a alloc in
  let bloc = vreg_loc cg b alloc in
  emit cg (Printf.sprintf "    mov rcx, %s" bloc);
  emit cg (Printf.sprintf "    mov rax, %s" aloc);
  emit cg "    cqo";
  emit cg "    idiv rcx";
  if starts_with_prefix "[" dst then
    emit cg (Printf.sprintf "    mov %s, rax" dst)
  else if dst <> "rax" then
    emit cg (Printf.sprintf "    mov %s, rax" dst)

let generate cg funcs allocs =
  Buffer.clear cg.output;
  emit cg "; Generated by L1S compiler";
  emit cg "section .text";
  emit cg "global _start";
  emit cg "";

  List.iter (fun func ->
    let alloc = try List.assoc func.f_name allocs
                with Not_found -> failwith (Printf.sprintf "no allocation for %s" func.f_name)
    in
    cg.current_var_slots <- func.f_num_var_slots;
    let var_stack_size = func.f_num_var_slots * 8 in
    let spill_stack_size = alloc.stack_size in
    let total_stack = var_stack_size + spill_stack_size in
    let aligned_size = max ((total_stack + 15) land (lnot 15)) 8 in

    if func.f_name = "main" then begin
      emit cg "_start:";
      emit cg "    call main_func";
      emit cg "    mov rdi, rax";
      emit cg "    mov rax, 60";
      emit cg "    syscall";
      emit cg "";
      emit cg "main_func:"
    end else
      emit cg (Printf.sprintf "%s:" func.f_name);

    emit cg "    push rbp";
    emit cg "    mov rbp, rsp";
    if aligned_size > 0 then
      emit cg (Printf.sprintf "    sub rsp, %d" aligned_size);

    (* Store params to their spill locations *)
    List.iter (fun param ->
      let loc = vreg_loc cg param.p_vreg alloc in
      match get_param_reg param.p_vreg func.f_params with
      | Some param_reg when starts_with_prefix "[" loc ->
        emit cg (Printf.sprintf "    mov %s, %s" loc param_reg)
      | Some param_reg when loc <> param_reg ->
        emit cg (Printf.sprintf "    mov %s, %s" loc param_reg)
      | _ -> ()
    ) func.f_params;

    let _phi_resolves = collect_phi_resolves func alloc in

    List.iter (fun block ->
      emit cg (Printf.sprintf ".%s:" block.label);
      List.iter (fun instr ->
        match instr with
        | Phi _ -> ()
        | _ -> emit_instr cg instr alloc
      ) block.instrs;

      (match block.terminator with
       | Ret (Some vreg) ->
         let loc = vreg_loc cg vreg alloc in
         if loc <> "rax" then emit cg (Printf.sprintf "    mov rax, %s" loc);
         emit cg "    mov rsp, rbp";
         emit cg "    pop rbp";
         emit cg "    ret"
       | Ret None ->
         emit cg "    mov rsp, rbp";
         emit cg "    pop rbp";
         emit cg "    ret"
       | Br target ->
         emit cg (Printf.sprintf "    jmp .%s" target)
       | BrCond (vreg, t, f) ->
         let loc = vreg_loc cg vreg alloc in
         emit cg (Printf.sprintf "    cmp %s, 0" loc);
         emit cg (Printf.sprintf "    jne .%s" t);
         emit cg (Printf.sprintf "    jmp .%s" f)
       | Unreachable ->
         emit cg "    ud2")
    ) func.f_blocks;

    if func.f_name = "main" then
      emit cg "main_func_end:";
    emit cg ""
  ) func.funcs;

  Buffer.contents cg.output
