type parser = {
  tokens: Token.spanned_token list;
  mutable pos: int;
  diags: Diagnostic.t list;
}

let make tokens = { tokens; pos = 0; diags = [] }

let peek p = if p.pos >= List.length p.tokens then (Token.Eof, Span.dummy ()) else List.nth p.tokens p.pos
let previous_span p = if p.pos = 0 then Span.dummy () else let (_, sp) = List.nth p.tokens (p.pos - 1) in sp
let advance p = let t = peek p in p.pos <- p.pos + 1; t
let match_token p tok = let (t, sp) = peek p in if t = tok then begin p.pos <- p.pos + 1; Some sp end else None
let expect p tok =
  let (t, sp) = peek p in
  if t = tok then begin p.pos <- p.pos + 1; sp end
  else begin
    let msg = Printf.sprintf "expected %s, got %s" (Token.token_to_string tok) (Token.token_to_string t) in
    Span.dummy ()
  end

let expect_ident p =
  let (t, sp) = peek p in
  match t with
  | Token.Ident name -> p.pos <- p.pos + 1; name
  | _ -> let msg = Printf.sprintf "expected identifier, got %s" (Token.token_to_string t) in "_"

(* Pratt parser helpers *)
let prefix_bp = function
  | Token.Minus -> 70 | Token.Bang -> 70 | Token.Ident _ | Token.NumLiteral _ -> 0
  | _ -> -1

let infix_bp = function
  | Token.Eq -> 5
  | Token.PipePipe -> 10
  | Token.AndAnd -> 15
  | Token.EqEq | Token.BangEq -> 20
  | Token.Lt | Token.Gt | Token.LtEq | Token.GtEq -> 25
  | Token.Plus | Token.Minus -> 30
  | Token.Star | Token.Slash -> 40
  | Token.OpenParen -> 80
  | _ -> -1

let rec parse_expr_bp p min_bp =
  let (t, sp) = peek p in
  let bp = prefix_bp t in
  if bp < 0 then failwith (Printf.sprintf "unexpected token: %s" (Token.token_to_string t));
  let lhs = match t with
    | Token.Minus -> p.pos <- p.pos + 1; Ast.Unary (Ast.Neg, parse_expr_bp p 70)
    | Token.Bang -> p.pos <- p.pos + 1; Ast.Unary (Ast.Not, parse_expr_bp p 70)
    | Token.NumLiteral s -> p.pos <- p.pos + 1; Ast.Int (Int64.of_string s)
    | Token.Ident s -> p.pos <- p.pos + 1; Ast.Ident s
    | _ -> advance p; Ast.Int 0L
  in
  parse_infix p lhs min_bp

and parse_infix p lhs min_bp =
  let (t, _) = peek p in
  let lbp = infix_bp t in
  if lbp < 0 || lbp < min_bp then lhs
  else begin
    p.pos <- p.pos + 1;
    match t with
    | Token.Eq ->
      let rhs = parse_expr_bp p 4 in
      parse_infix p (Ast.Assign (lhs, rhs)) min_bp
    | Token.PipePipe ->
      let rhs = parse_expr_bp p 11 in
      parse_infix p (Ast.Binary (Ast.Or, lhs, rhs)) min_bp
    | Token.AndAnd ->
      let rhs = parse_expr_bp p 16 in
      parse_infix p (Ast.Binary (Ast.And, lhs, rhs)) min_bp
    | Token.EqEq ->
      let rhs = parse_expr_bp p 21 in
      parse_infix p (Ast.Binary (Ast.Eq, lhs, rhs)) min_bp
    | Token.BangEq ->
      let rhs = parse_expr_bp p 21 in
      parse_infix p (Ast.Binary (Ast.Neq, lhs, rhs)) min_bp
    | Token.Lt ->
      let rhs = parse_expr_bp p 26 in
      parse_infix p (Ast.Binary (Ast.Lt, lhs, rhs)) min_bp
    | Token.Gt ->
      let rhs = parse_expr_bp p 26 in
      parse_infix p (Ast.Binary (Ast.Gt, lhs, rhs)) min_bp
    | Token.LtEq ->
      let rhs = parse_expr_bp p 26 in
      parse_infix p (Ast.Binary (Ast.Le, lhs, rhs)) min_bp
    | Token.GtEq ->
      let rhs = parse_expr_bp p 26 in
      parse_infix p (Ast.Binary (Ast.Ge, lhs, rhs)) min_bp
    | Token.Plus ->
      let rhs = parse_expr_bp p 31 in
      parse_infix p (Ast.Binary (Ast.Add, lhs, rhs)) min_bp
    | Token.Minus ->
      let rhs = parse_expr_bp p 31 in
      parse_infix p (Ast.Binary (Ast.Sub, lhs, rhs)) min_bp
    | Token.Star ->
      let rhs = parse_expr_bp p 41 in
      parse_infix p (Ast.Binary (Ast.Mul, lhs, rhs)) min_bp
    | Token.Slash ->
      let rhs = parse_expr_bp p 41 in
      parse_infix p (Ast.Binary (Ast.Div, lhs, rhs)) min_bp
    | Token.OpenParen ->
      let args = parse_call_args p in
      (match lhs with Ast.Ident name -> parse_infix p (Ast.Call (name, args)) min_bp | _ -> lhs)
    | _ -> lhs
  end

and parse_call_args p =
  let rec loop acc =
    let (t, _) = peek p in
    if t = Token.CloseParen then (p.pos <- p.pos + 1; List.rev acc)
    else begin
      let expr = parse_expr_bp p 0 in
      let (t', _) = peek p in
      if t' = Token.Comma then p.pos <- p.pos + 1;
      loop (expr :: acc)
    end
  in
  loop []

let parse_expr p = parse_expr_bp p 0

(* Type parsing *)
let parse_type p =
  let (t, sp) = peek p in
  match t with
  | Token.OpenBracket ->
    p.pos <- p.pos + 1;
    let size = parse_expr p in
    let _ = expect p Token.CloseBracket in
    let inner = parse_type p in
    Ast.Array (size, inner)
  | Token.At ->
    p.pos <- p.pos + 1;
    let inner = parse_type p in
    Ast.Ptr inner
  | Token.I32 -> p.pos <- p.pos + 1; Ast.Prim Ast.I32
  | Token.I64 -> p.pos <- p.pos + 1; Ast.Prim Ast.I64
  | Token.U32 -> p.pos <- p.pos + 1; Ast.Prim Ast.U32
  | Token.U64 -> p.pos <- p.pos + 1; Ast.Prim Ast.U64
  | Token.I8 -> p.pos <- p.pos + 1; Ast.Prim Ast.I8
  | Token.U8 -> p.pos <- p.pos + 1; Ast.Prim Ast.U8
  | Token.Bool -> p.pos <- p.pos + 1; Ast.Prim Ast.Bool
  | Token.Void -> p.pos <- p.pos + 1; Ast.Prim Ast.Void
  | Token.Ident name -> p.pos <- p.pos + 1; Ast.Named name
  | _ -> failwith (Printf.sprintf "expected type, got %s" (Token.token_to_string t))

(* Block: { stmts } *)
let parse_block p =
  expect p Token.OpenBrace;
  let rec loop acc =
    let (t, _) = peek p in
    if t = Token.CloseBrace then (p.pos <- p.pos + 1; Ast.{ stmts = List.rev acc })
    else loop (parse_statement p :: acc)
  in
  loop []

(* Statement *)
and parse_statement p =
  let (t, sp) = peek p in
  match t with
  | Token.Return ->
    p.pos <- p.pos + 1;
    let expr = parse_expr p in
    let _ = expect p Token.Semicolon in
    Ast.Return expr
  | Token.Let -> parse_var_decl p
  | Token.If -> parse_if p
  | Token.While -> parse_while p
  | Token.OpenBrace -> parse_block p
  | _ ->
    let expr = parse_expr p in
    let _ = expect p Token.Semicolon in
    Ast.Expr expr

and parse_var_decl p =
  let _ = expect p Token.Let in
  let name = expect_ident p in
  let _ = expect p Token.Colon in
  let typ = parse_type p in
  let init =
    if match_token p Token.Eq <> None then Some (parse_expr p)
    else None
  in
  let _ = expect p Token.Semicolon in
  Ast.VarDecl { name; type_ = typ; init }

and parse_if p =
  let _ = expect p Token.If in
  let _ = expect p Token.OpenParen in
  let cond = parse_expr p in
  let _ = expect p Token.CloseParen in
  let then_block = parse_block p in
  let else_branch =
    if match_token p Token.Else <> None then
      let (t', _) = peek p in
      Some (if t' = Token.If then parse_if p else Ast.Block (parse_block p))
    else None
  in
  Ast.If { cond; then_block; else_branch }

and parse_while p =
  let _ = expect p Token.While in
  let _ = expect p Token.OpenParen in
  let cond = parse_expr p in
  let _ = expect p Token.CloseParen in
  let body = parse_block p in
  Ast.While { cond; body }

(* Declaration: func name (ret_type , params?) { body } *)
let parse_func_decl p =
  let _ = expect p Token.Func in
  let name = expect_ident p in
  let _ = expect p Token.OpenParen in
  let return_type = parse_type p in
  let params =
    if match_token p Token.Comma <> None then begin
      let rec loop acc =
        let (t, _) = peek p in
        if t = Token.CloseParen then List.rev acc
        else begin
          let pname = expect_ident p in
          let _ = expect p Token.Colon in
          let ptype = parse_type p in
          let acc' = Ast.{ name = pname; type_ = ptype } :: acc in
          let _ = if match_token p Token.Comma = None then () else () in
          loop acc'
        end
      in
      loop []
    end
    else []
  in
  let _ = expect p Token.CloseParen in
  let body = parse_block p in
  Ast.Func { name; return_type; params; body }

let parse_program tokens =
  let p = make tokens in
  let rec loop acc =
    let (t, _) = peek p in
    if t = Token.Eof then Ast.{ decls = List.rev acc }
    else begin
      let decl = match t with
        | Token.Func -> parse_func_decl p
        | _ -> p.pos <- p.pos + 1; Ast.Func { name = "_"; return_type = Ast.Prim Ast.Void; params = []; body = { stmts = [] } }
      in
      loop (decl :: acc)
    end
  in
  loop []
