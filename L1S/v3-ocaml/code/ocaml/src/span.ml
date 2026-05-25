type span = { start_pos: int; end_pos: int }

let dummy = { start_pos = 0; end_pos = 0 }

let make st en = { start_pos = st; end_pos = en }

let to_string s =
  if s.start_pos = s.end_pos then
    Printf.sprintf "position %d" s.start_pos
  else
    Printf.sprintf "positions %d-%d" s.start_pos s.end_pos
