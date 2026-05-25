open Ir

module VRegMap = Map.Make(Int)

type phys_reg =
  | Rax | Rcx | Rdx | Rbx | Rsi | Rdi
  | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

let all_regs = [
  Rax; Rcx; Rdx; Rbx; Rsi; Rdi;
  R8; R9; R10; R11; R12; R13; R14; R15;
]

let arg_regs = [Rdi; Rsi; Rdx; Rcx; R8; R9]

type live_interval = {
  vreg: int;
  start_: int;
  end_: int;
}

type allocation = {
  reg_map: phys_reg VRegMap.t;
  spill_slots: int VRegMap.t;
  stack_size: int;
}

type allocator = unit

let create () = ()

let compute_live_intervals func =
  let defs = ref VRegMap.empty in
  let uses = ref VRegMap.empty in
  let instr_counter = ref 0 in

  List.iter (fun block ->
    List.iter (fun instr ->
      let u = uses_of_instr instr in
      List.iter (fun v ->
        let existing = match VRegMap.find_opt v !uses with
          | Some l -> l | None -> []
        in
        uses := VRegMap.add v (existing @ [!instr_counter]) !uses
      ) u;
      (match dest_of_instr instr with
       | Some d -> defs := VRegMap.add d !instr_counter !defs
       | None -> ());
      instr_counter := !instr_counter + 1
    ) block.instrs;
    (match block.terminator with
     | Ret (Some v) ->
       let existing = match VRegMap.find_opt v !uses with
         | Some l -> l | None -> []
       in
       uses := VRegMap.add v (existing @ [!instr_counter]) !uses
     | BrCond (v, _, _) ->
       let existing = match VRegMap.find_opt v !uses with
         | Some l -> l | None -> []
       in
       uses := VRegMap.add v (existing @ [!instr_counter]) !uses
     | _ -> ());
    instr_counter := !instr_counter + 1
  ) func.f_blocks;

  let intervals = ref [] in
  VRegMap.iter (fun vreg def ->
    let last_use = match VRegMap.find_opt vreg !uses with
      | Some l -> List.fold_left max def l
      | None -> def
    in
    intervals := { vreg; start_ = def; end_ = last_use } :: !intervals
  ) !defs;
  VRegMap.iter (fun vreg use_list ->
    if not (VRegMap.mem vreg !defs) then begin
      let last_use = List.fold_left max 0 use_list in
      intervals := { vreg; start_ = 0; end_ = last_use } :: !intervals
    end
  ) !uses;

  List.sort (fun a b -> compare a.start_ b.start_) !intervals

let get_spill_slot counter =
  let s = !counter in
  counter := !counter + 1;
  s

let expire_intervals current_start active reg_map =
  let (keep, _) = List.partition (fun (e, v, r) ->
    if e <= current_start then begin
      reg_map := VRegMap.remove v !reg_map;
      false
    end else true
  ) !active in
  active := keep

let allocate func =
  let intervals = compute_live_intervals func in
  let sorted = List.sort (fun a b -> compare a.start_ b.start_) intervals in
  let reg_map = ref VRegMap.empty in
  let spill_slots = ref VRegMap.empty in
  let next_spill_slot = ref 0 in
  let active = ref [] in

  List.iter (fun interval ->
    expire_intervals interval.start_ active reg_map;

    if List.length !active < List.length all_regs then begin
      let reg = List.nth all_regs (List.length !active) in
      reg_map := VRegMap.add interval.vreg reg !reg_map;
      active := (interval.end_, interval.vreg, reg) :: !active
    end else begin
      let spill_candidate =
        let (max_end, max_vreg, _) = List.fold_left (fun (me, mv, _) (e, v, _) ->
          if e > me then (e, v, true) else (me, mv, true)
        ) (0, 0, false) !active
        in max_vreg
      in
      let candidate_interval = List.find (fun i -> i.vreg = spill_candidate) intervals in
      if candidate_interval.end_ > interval.end_ then begin
        let reg = VRegMap.find spill_candidate !reg_map in
        reg_map := VRegMap.remove spill_candidate !reg_map;
        let slot = get_spill_slot next_spill_slot in
        spill_slots := VRegMap.add spill_candidate slot !spill_slots;
        reg_map := VRegMap.add interval.vreg reg !reg_map;
        active := List.filter (fun (_, v, _) -> v <> spill_candidate) !active;
        active := (interval.end_, interval.vreg, reg) :: !active
      end else begin
        let slot = get_spill_slot next_spill_slot in
        spill_slots := VRegMap.add interval.vreg slot !spill_slots
      end
    end
  ) sorted;

  (* Any remaining unassigned vregs get a spill slot *)
  List.iter (fun interval ->
    if not (VRegMap.mem interval.vreg !reg_map) &&
       not (VRegMap.mem interval.vreg !spill_slots) then begin
      let slot = get_spill_slot next_spill_slot in
      spill_slots := VRegMap.add interval.vreg slot !spill_slots
    end
  ) intervals;

  { reg_map = !reg_map; spill_slots = !spill_slots; stack_size = !next_spill_slot * 8 }

let phys_name = function
  | Rax -> "rax" | Rcx -> "rcx" | Rdx -> "rdx" | Rbx -> "rbx"
  | Rsi -> "rsi" | Rdi -> "rdi"
  | R8 -> "r8" | R9 -> "r9" | R10 -> "r10" | R11 -> "r11"
  | R12 -> "r12" | R13 -> "r13" | R14 -> "r14" | R15 -> "r15"

let arg_reg_name i =
  if i < List.length arg_regs then phys_name (List.nth arg_regs i)
  else "rdi"
