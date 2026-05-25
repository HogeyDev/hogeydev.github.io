type tc = {
  syms: Symbols.t;
  mutable diags: Diagnostic.t list;
  source: Span.source_file;
}

let make source = { syms = Symbols.create (); diags = []; source }

let error tc span msg =
  tc.diags <- Diagnostic.error span msg :: tc.diags

(* Phase 1: collect declarations *)
let collect_decls tc prog =
  List.iter (fun decl ->
    match decl with
    | Ast.Func f ->
      let param_types = List.map (fun p -> Symbols.ast_type_to_ir p.Ast.type_) f.Ast.params in
      let ret_type = Symbols.ast_type_to_ir f.Ast.return_type in
      Symbols.insert tc.syms f.Ast.name { typ = ret_type; kind = Symbols.Func (param_types, ret_type) }
  ) prog.Ast.decls

(* Phase 2: check function bodies *)
let rec check_expr tc expr =
  match expr with
  | Ast.Int _ -> Some Symbols.I32
  | Ast.Bool _ -> Some Symbols.Bool
  | Ast.Ident name ->
    (match Symbols.lookup tc.syms name with
     | Some s -> Some s.Symbols.typ
     | None -> error tc (Span.dummy ()) (Printf.sprintf "undefined variable: %s" name); None)
  | Ast.Binary (op, l, r) ->
    let lt = check_expr tc l and rt = check_expr tc r in
    (match lt, rt with
     | Some t1, Some t2 when t1 = t2 -> Some t1
     | _ -> error tc (Span.dummy ()) "type mismatch in binary expression"; None)
  | Ast.Unary (_, e) -> check_expr tc e
  | Ast.Call (name, args) ->
    (match Symbols.lookup tc.syms name with
     | Some { kind = Symbols.Func (param_types, ret_type); _ } ->
       if List.length args <> List.length param_types then
         error tc (Span.dummy ()) (Printf.sprintf "expected %d args, got %d" (List.length param_types) (List.length args));
       Some ret_type
     | _ -> error tc (Span.dummy ()) (Printf.sprintf "undefined function: %s" name); None)
  | Ast.Cast (t, e) ->
    let _ = check_expr tc e in
    Some (Symbols.ast_type_to_ir t)
  | Ast.Assign (l, r) ->
    let lt = check_expr tc l and rt = check_expr tc r in
    (match lt, rt with
     | Some t1, Some t2 when t1 = t2 -> Some t1
     | _ -> error tc (Span.dummy ()) "type mismatch in assignment"; None)

let rec check_stmt tc stmt =
  match stmt with
  | Ast.Return expr -> let _ = check_expr tc expr in ()
  | Ast.VarDecl v ->
    let vt = Symbols.ast_type_to_ir v.Ast.type_ in
    (match v.Ast.init with
     | Some e ->
       let et = check_expr tc e in
       (match et with
        | Some t when t <> vt -> error tc (Span.dummy ()) "type mismatch in variable declaration"
        | _ -> ())
     | None -> ());
    Symbols.insert tc.syms v.Ast.name { typ = vt; kind = Symbols.Var vt }
  | Ast.If { cond; then_block; else_branch; _ } ->
    let _ = check_expr tc cond in
    check_block tc then_block;
    Option.iter (fun s -> check_stmt tc s) else_branch
  | Ast.While { cond; body; _ } ->
    let _ = check_expr tc cond in
    check_block tc body
  | Ast.Block b -> check_block tc b
  | Ast.Expr e -> let _ = check_expr tc e in ()

and check_block tc block =
  Symbols.enter_scope tc.syms;
  List.iter (fun s -> check_stmt tc s) block.Ast.stmts;
  Symbols.exit_scope tc.syms

let check_func tc func =
  Symbols.enter_scope tc.syms;
  List.iter (fun p -> Symbols.insert tc.syms p.Ast.name { typ = Symbols.ast_type_to_ir p.Ast.type_; kind = Symbols.Var (Symbols.ast_type_to_ir p.Ast.type_) }) func.Ast.params;
  check_block tc func.Ast.body;
  Symbols.exit_scope tc.syms

let check_program source prog =
  let tc = make source in
  collect_decls tc prog;
  List.iter (fun decl ->
    match decl with Ast.Func f -> check_func tc f) prog.Ast.decls;
  (List.rev tc.diags)
