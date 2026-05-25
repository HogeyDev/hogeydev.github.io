open Ir

module VRegSet = Set.Make(Int)
module VRegMap = Map.Make(Int)

type ctx = {
  mutable modified: bool;
}

let create () = { modified = false }

let try_propagate instr =
  match instr with
  | Copy (d, s) -> Some (d, s)
  | _ -> None

let rec substitute uses from_vreg to_vreg =
  List.map (fun u -> if u = from_vreg then to_vreg else u) uses

let run_func ctx func =
  (* Simple copy propagation *)
  let subst_map = ref VRegMap.empty in
  let new_blocks = List.map (fun block ->
    let new_instrs = List.filter_map (fun instr ->
      (* Build substitution from copy propagation *)
      (match instr with
       | Copy (d, s) ->
         if not (is_side_effect instr) then
           subst_map := VRegMap.add d s !subst_map
       | _ -> ());
      (* Try to propagate *)
      let rec resolve v =
        match VRegMap.find_opt v !subst_map with
        | Some v' -> resolve v'
        | None -> v
      in
      let subst_instr u = resolve u in
      Some instr
    ) block.instrs in
    { block with instrs = new_instrs }
  ) func.f_blocks in
  { func with f_blocks = new_blocks }

let run_all ctx funcs =
  List.map (run_func ctx) funcs
