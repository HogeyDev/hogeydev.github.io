open Ast
open Token
open Span

type parser = {
  lexer: Lexer.lexer;
  diags: Diag.t;
  current: token ref;
  peek: token ref;
}

let create source diags =
  let lx = Lexer.create source in
  let p = {
    lexer = lx;
    diags;
    current = ref { kind = TEOF; lexeme = ""; span = dummy };
    peek = ref { kind = TEOF; lexeme = ""; span = dummy };
  } in
  p.current := Lexer.next_token lx;
  p.peek := Lexer.next_token lx;
  p

let advance p =
  p.current := !(p.peek);
  p.peek := Lexer.next_token p.lexer

let check p k = !(p.current).kind = k

let consume p k =
  if check p k then begin
    let t = !(p.current) in
    advance p;
    t
  end else
    failwith (Printf.sprintf "expected %s, got %s"
      (kind_to_string k) (kind_to_string !(p.current).kind))

let expect p k =
  if check p k then advance p
  else begin
    Diag.error p.diags ~span:!(p.current).span
      (Printf.sprintf "expected '%s', got '%s'"
        (kind_to_string k) (kind_to_string !(p.current).kind));
    advance p
  end

(* Pratt parsing helpers *)

type prefix_fn = parser -> expr
type infix_fn = parser -> expr -> expr

type bp = int

let prefix_bp = function
  | TMinus -> Some ((), 70)
  | TNot -> Some ((), 70)
  | TInt _ | TTrue | TFalse | TIdent _ | TLParen -> Some ((), 0)
  | _ -> None

let infix_bp = function
  | TPlus -> Some (50, 51)
  | TMinus -> Some (50, 51)
  | TStar -> Some (60, 61)
  | TSlash -> Some (60, 61)
  | TEqEq -> Some (30, 31)
  | TNeq -> Some (30, 31)
  | TLt -> Some (40, 41)
  | TGt -> Some (40, 41)
  | TLe -> Some (40, 41)
  | TGe -> Some (40, 41)
  | TAnd -> Some (20, 21)
  | TOr -> Some (10, 11)
  | _ -> None

let rec parse_expr p min_bp =
  let tok = !(p.current) in
  let ((), rbp) = match prefix_bp tok.kind with
    | Some v -> v
    | None ->
      Diag.error p.diags ~span:tok.span "expected expression";
      advance p;
      ((), 0)
  in
  let mut_lhs = parse_prefix p tok in
  parse_expr_inner p mut_lhs min_bp

and parse_prefix p tok =
  match tok.kind with
  | TInt n -> advance p; (EInt n, tok.span)
  | TTrue -> advance p; (EBool true, tok.span)
  | TFalse -> advance p; (EBool false, tok.span)
  | TIdent s -> advance p; (EIdent s, tok.span)
  | TLParen ->
    advance p;
    let e = parse_expr p 0 in
    consume p TRParen;
    e
  | TMinus ->
    advance p;
    let rhs = parse_expr p 70 in
    (EUnary (Neg, rhs), make_span' tok.span rhs)
  | TNot ->
    advance p;
    let rhs = parse_expr p 70 in
    (EUnary (Not, rhs), make_span' tok.span rhs)
  | _ ->
    Diag.error p.diags ~span:tok.span "unexpected token in expression";
    advance p;
    (EInt 0L, tok.span)

and parse_expr_inner p lhs min_bp =
  let rec go lhs =
    let tok = !(p.current) in
    match infix_bp tok.kind with
    | Some (l, r) when l >= min_bp ->
      advance p;
      let rhs = parse_expr p r in
      let span = make_span' (snd lhs) (snd rhs) in
      let op = match tok.kind with
        | TPlus -> Add | TMinus -> Sub | TStar -> Mul | TSlash -> Div
        | TEqEq -> Eq | TNeq -> Neq
        | TLt -> Lt | TGt -> Gt | TLe -> Le | TGe -> Ge
        | TAnd -> And | TOr -> Or
        | _ -> failwith "unexpected infix"
      in
      go (EBinary (op, lhs, rhs), span)
    | _ -> lhs
  in
  go lhs

and make_span' (s1: span) (s2: span) = make s1.start_pos s2.end_pos

let parse_type p =
  let tok = !(p.current) in
  match tok.kind with
  | TI32 -> advance p; TPrim TI32
  | TBool -> advance p; TPrim TBool
  | TVoid -> advance p; TPrim TVoid
  | _ ->
    Diag.error p.diags ~span:tok.span "expected type";
    advance p;
    TPrim TI32

let consume_ident p =
  match !(p.current).kind with
  | TIdent s -> let t = !(p.current) in advance p; t
  | _ -> failwith "expected identifier"

let parse_param p =
  let name_tok = consume_ident p in
  let name = name_tok.lexeme in
  expect p TColon;
  let typ = parse_type p in
  { p_name = name; p_name_span = name_tok.span; p_type = typ; p_type_span = !(p.current).span }

let rec parse_stmt p =
  let tok = !(p.current) in
  let start = tok.span.start_pos in
  match tok.kind with
  | TReturn ->
    advance p;
    let e_opt =
      if check p TSemicolon then None
      else Some (parse_expr p 0)
    in
    expect p TSemicolon;
    let end_ = !(p.current).span.end_pos in
    (SReturn e_opt, make start end_)
  | TLet ->
    advance p;
    let _is_mut = if check p TMut then begin advance p; true end else false in
    let name_tok = consume_ident p in
    expect p TColon;
    let typ = parse_type p in
    let init =
      if check p TEquals then begin
        advance p;
        Some (parse_expr p 0)
      end else None
    in
    expect p TSemicolon;
    let end_ = !(p.current).span.end_pos in
    (SVarDecl (name_tok.lexeme, typ, init), make start end_)
  | TIf ->
    advance p;
    expect p TLParen;
    let cond = parse_expr p 0 in
    expect p TRParen;
    let then_body = parse_block p in
    let else_body =
      if check p TElse then begin
        advance p;
        Some (parse_block p)
      end else None
    in
    let end_ = !(p.current).span.end_pos in
    (SIf (cond, then_body, else_body), make start end_)
  | TWhile ->
    advance p;
    expect p TLParen;
    let cond = parse_expr p 0 in
    expect p TRParen;
    let body = parse_block p in
    let end_ = !(p.current).span.end_pos in
    (SWhile (cond, body), make start end_)
  | TLBrace ->
    let stmts = parse_block p in
    let end_ = !(p.current).span.end_pos in
    (SBlock stmts, make start end_)
  | TIdent _ ->
    let name = tok.lexeme in
    advance p;
    if check p TEquals then begin
      advance p;
      let rhs = parse_expr p 0 in
      expect p TSemicolon;
      let end_ = !(p.current).span.end_pos in
      (SAssign (name, rhs), make start end_)
    end else begin
      let expr = parse_expr_or_ident p name tok.span in
      expect p TSemicolon;
      let end_ = !(p.current).span.end_pos in
      (SExpr expr, make start end_)
    end
  | _ ->
    Diag.error p.diags ~span:tok.span "unexpected token in statement";
    advance p;
    (SExpr (EInt 0L, tok.span), tok.span)

and parse_expr_or_ident p name name_span =
  match !(p.current).kind with
  | TLParen ->
    advance p;
    let args = parse_call_args p in
    consume p TRParen;
    let end_ = !(p.current).span.end_pos in
    (ECall (name, args), make name_span.start_pos end_)
  | _ ->
    (EIdent name, name_span)

and parse_call_args p =
  let rec loop acc =
    if check p TRParen then List.rev acc
    else begin
      let e = parse_expr p 0 in
      if check p TComma then advance p;
      loop (e :: acc)
    end
  in
  loop []

and parse_block p =
  consume p TLBrace;
  let rec loop acc =
    if check p TRBrace then begin
      advance p;
      List.rev acc
    end else begin
      let s = parse_stmt p in
      loop (s :: acc)
    end
  in
  loop []

let rec parse_func_decl p =
  consume p TFunc;
  let name_tok = consume_ident p in
  consume p TLParen;
  let params =
    if check p TRParen then []
    else begin
      let rec loop acc =
        let param = parse_param p in
        if check p TComma then begin advance p; loop (param :: acc) end
        else List.rev (param :: acc)
      in
      loop []
    end
  in
  consume p TRParen;
  let return_type =
    if check p TArrow then begin
      advance p;
      parse_type p
    end else TPrim TVoid
  in
  let body = parse_block p in
  let span = make name_tok.span.start_pos !(p.current).span.end_pos in
  AFunc {
    f_name = name_tok.lexeme; f_name_span = name_tok.span;
    f_params = params; f_return_type = return_type;
    f_return_span = !(p.current).span; f_body = body; f_span = span;
  }

let parse_decl p =
  match !(p.current).kind with
  | TFunc -> parse_func_decl p
  | _ ->
    Diag.error p.diags ~span:!(p.current).span "expected declaration";
    advance p;
    let f = {
      f_name = ""; f_name_span = dummy; f_params = [];
      f_return_type = TPrim TVoid; f_return_span = dummy;
      f_body = []; f_span = dummy;
    } in
    AFunc f

let parse_program p =
  let rec loop acc =
    if check p TEOF then List.rev acc
    else begin
      let d = parse_decl p in
      loop (d :: acc)
    end
  in
  let decls = loop [] in
  { decls }
