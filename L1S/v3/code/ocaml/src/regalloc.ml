type allocator = {
  mutable next_slot: int;
  slots: (int, int) Hashtbl.t;
  mutable frame_size: int;
}

let create () = { next_slot = 0; slots = Hashtbl.create 16; frame_size = 0 }

let alloc_slot alloc =
  let slot = alloc.next_slot in
  alloc.next_slot <- slot + 8;
  slot

let round_up_to_16 n = (n + 15) land (-16)

let allocate_function func =
  let alloc = create () in
  let _ = List.iter (fun block ->
    List.iter (fun instr ->
      let vregs = match instr with
        | Ir.Const (v, _, _) | Ir.Add (v, _, _) | Ir.Sub (v, _, _)
        | Ir.Mul (v, _, _) | Ir.Div (v, _, _) | Ir.Eq (v, _, _) | Ir.Lt (v, _, _)
        | Ir.And (v, _, _) | Ir.Or (v, _, _) | Ir.Not (v, _) | Ir.Neg (v, _)
        | Ir.Load (v, _, _) | Ir.Call (v, _, _, _) | Ir.Alloc (v, _) -> [v]
        | Ir.Store (_, v, _) -> [v]
      in
      List.iter (fun v ->
        if not (Hashtbl.mem alloc.slots v) then
          Hashtbl.add alloc.slots v (alloc_slot alloc)
      ) vregs
    ) block.Ir.instrs
  ) func.Ir.blocks in
  Hashtbl.iter (fun v slot -> Hashtbl.add func.Ir.slot_map v slot) alloc.slots;
  func.Ir.frame_size <- round_up_to_16 alloc.next_slot
