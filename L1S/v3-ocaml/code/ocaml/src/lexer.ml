type lexer = {
  source: string;
  pos: int ref;
  line: int ref;
  col: int ref;
}

let create source = {
  source;
  pos = ref 0;
  line = ref 1;
  col = ref 0;
}

let peek lx =
  if !(lx.pos) >= String.length lx.source then None
  else Some lx.source.[!(lx.pos)]

let advance lx =
  match peek lx with
  | Some c ->
    lx.pos := !(lx.pos) + 1;
    if c = '\n' then begin lx.line := !(lx.line) + 1; lx.col := 0 end
    else lx.col := !(lx.col) + 1;
    Some c
  | None -> None

let make_span lx start_pos = Span.make start_pos !(lx.pos)

let rec skip_whitespace_and_comments lx =
  match peek lx with
  | Some (' ' | '\t' | '\n' | '\r') -> advance lx |> ignore; skip_whitespace_and_comments lx
  | Some '/' ->
    let saved = !(lx.pos) in
    advance lx |> ignore;
    (match peek lx with
     | Some '/' ->
       advance lx |> ignore;
       skip_line_comment lx;
       skip_whitespace_and_comments lx
     | _ ->
       lx.pos := saved;
       ())
  | _ -> ()

and skip_line_comment lx =
  match peek lx with
  | Some ('\n' | '\r') -> ()
  | Some _ -> advance lx |> ignore; skip_line_comment lx
  | None -> ()

let read_number lx start_pos =
  let buf = Buffer.create 8 in
  let rec loop () =
    match peek lx with
    | Some c when c >= '0' && c <= '9' ->
      Buffer.add_char buf c;
      advance lx |> ignore;
      loop ()
    | _ -> ()
  in
  loop ();
  let n = Int64.of_string (Buffer.contents buf) in
  let span = make_span lx start_pos in
  ({ kind = TInt n; lexeme = Buffer.contents buf; span }, n)

let read_ident_or_keyword lx start_pos =
  let buf = Buffer.create 8 in
  let rec loop () =
    match peek lx with
    | Some c when (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || (c >= '0' && c <= '9') ->
      Buffer.add_char buf c;
      advance lx |> ignore;
      loop ()
    | _ -> ()
  in
  loop ();
  let s = Buffer.contents buf in
  let kind = match s with
    | "func" -> TFunc | "let" -> TLet | "mut" -> TMut
    | "if" -> TIf | "else" -> TElse | "while" -> TWhile
    | "return" -> TReturn | "true" -> TTrue | "false" -> TFalse
    | "i32" -> TI32 | "bool" -> TBool | "void" -> TVoid
    | _ -> TIdent s
  in
  { kind; lexeme = s; span = make_span lx start_pos }

let rec next_token lx =
  skip_whitespace_and_comments lx;
  let start_pos = !(lx.pos) in
  match advance lx with
  | None -> { kind = TEOF; lexeme = ""; span = make_span lx start_pos }
  | Some c ->
    let kind, lexeme =
      match c with
      | '(' -> TLParen, "(" | ')' -> TRParen, ")"
      | '{' -> TLBrace, "{" | '}' -> TRBrace, "}"
      | ':' -> TColon, ":" | ',' -> TComma, ","
      | '+' -> TPlus, "+" | '-' ->
        (match peek lx with
         | Some '>' -> advance lx |> ignore; TArrow, "->"
         | _ -> TMinus, "-")
      | '*' -> TStar, "*" | '/' -> TSlash, "/"
      | '!' ->
        (match peek lx with
         | Some '=' -> advance lx |> ignore; TNeq, "!="
         | _ -> TNot, "!")
      | '=' ->
        (match peek lx with
         | Some '=' -> advance lx |> ignore; TEqEq, "=="
         | _ -> TEquals, "=")
      | '<' ->
        (match peek lx with
         | Some '=' -> advance lx |> ignore; TLe, "<="
         | _ -> TLt, "<")
      | '>' ->
        (match peek lx with
         | Some '=' -> advance lx |> ignore; TGe, ">="
         | _ -> TGt, ">")
      | '&' ->
        (match peek lx with
         | Some '&' -> advance lx |> ignore; TAnd, "&&"
         | _ -> failwith "unexpected '&'")
      | '|' ->
        (match peek lx with
         | Some '|' -> advance lx |> ignore; TOr, "||"
         | _ -> failwith "unexpected '|'")
      | ';' -> TSemicolon, ";"
      | c when c >= '0' && c <= '9' ->
        let tok, _ = read_number lx start_pos in
        tok.kind, tok.lexeme
      | c when (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' ->
        let tok = read_ident_or_keyword lx start_pos in
        tok.kind, tok.lexeme
      | _ -> failwith (Printf.sprintf "unexpected character '%c'" c)
    in
    { kind; lexeme; span = make_span lx start_pos }
