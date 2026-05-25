type span = { start: int; end: int }

let make s e = { start = s; end = e }
let dummy () = { start = 0; end = 0 }

type source_file = {
  filename: string;
  content: string;
  line_starts: int array;
}

let make_source_file filename content =
  let ls = Array.of_list (0 :: List.init (String.length content)
    (fun i -> if i < String.length content && content.[i] = '\n' then i + 1 else -1)
    |> List.filter (fun x -> x >= 0)) in
  { filename; content; line_starts = ls }

let line_col sf pos =
  let rec search lo hi =
    if lo > hi then lo - 1
    else
      let mid = (lo + hi) / 2 in
      if sf.line_starts.(mid) <= pos then search (mid + 1) hi
      else search lo (mid - 1)
  in
  let line = search 0 (Array.length sf.line_starts - 1) in
  let line_start = sf.line_starts.(line) in
  (line + 1, pos - line_start + 1)

let format_location sf sp =
  let l, c = line_col sf sp.start in
  Printf.sprintf "%s:%d:%d" sf.filename l c

let format_underline sf sp =
  let l, c = line_col sf sp.start in
  let line_start = sf.line_starts.(l - 1) in
  let line_end =
    let n = String.length sf.content in
    let rec go i = if i >= n || sf.content.[i] = '\n' then i else go (i + 1) in
    go line_start
  in
  let line = String.sub sf.content line_start (line_end - line_start) in
  let end_col = min (sp.end - sp.start) (String.length line - c + 1) in
  Printf.sprintf " %s\n%s\n%s^" (format_location sf sp) line
    (String.make (c - 1) ' ' ^ String.make (max end_col 1) '^')
