open Ast
open Symbols

type ctx = {
  symbols: Symbols.t;
  diags: Diag.t;
}

let create diags = { symbols = Symbols.create (); diags }

let check_prog ctx prog =
  (* First pass: register all function signatures *)
  List.iter (fun decl ->
    match decl with
    | AFunc f ->
      let param_types = List.map (fun p -> ast_type_to_sym p.p_type) f.f_params in
      let ret_type = ast_type_to_sym f.f_return_type in
      Symbols.insert ctx.symbols f.f_name (SymFunc (param_types, ret_type))
  ) prog.decls;

  (* Second pass: type-check function bodies *)
  List.iter (fun decl ->
    match decl with
    | AFunc f -> check_func ctx f
  ) prog.decls

and check_func ctx f =
  Symbols.enter_scope ctx.symbols;
  List.iter (fun p ->
    Symbols.insert ctx.symbols p.p_name (ast_type_to_sym p.p_type)
  ) f.f_params;

  (* Check each statement *)
  List.iter (fun stmt -> check_stmt ctx stmt) f.f_body;

  Symbols.exit_scope ctx.symbols

and check_stmt ctx (sk, _) =
  match sk with
  | SReturn (Some e) -> ignore (infer_expr ctx e)
  | SReturn None -> ()
  | SAssign (name, rhs) ->
    ignore (infer_expr ctx rhs)
  | SVarDecl (name, typ, init) ->
    (match init with
     | Some e -> ignore (infer_expr ctx e)
     | None -> ());
    Symbols.insert ctx.symbols name (ast_type_to_sym typ)
  | SExpr e -> ignore (infer_expr ctx e)
  | SIf (cond, then_, else_) ->
    ignore (infer_expr ctx cond);
    List.iter (fun s -> check_stmt ctx s) then_;
    (match else_ with Some es -> List.iter (fun s -> check_stmt ctx s) es | None -> ())
  | SWhile (cond, body) ->
    ignore (infer_expr ctx cond);
    List.iter (fun s -> check_stmt ctx s) body
  | SBlock stmts ->
    Symbols.enter_scope ctx.symbols;
    List.iter (fun s -> check_stmt ctx s) stmts;
    Symbols.exit_scope ctx.symbols

and infer_expr ctx (ek, _) =
  match ek with
  | EInt _ -> SymPrim TI32
  | EBool _ -> SymPrim TBool
  | EIdent name ->
    (match Symbols.lookup ctx.symbols name with
     | Some st -> st
     | None ->
       Diag.error ctx.diags (Printf.sprintf "undefined variable '%s'" name);
       SymPrim TI32)
  | EBinary (op, lhs, rhs) ->
    let lt = infer_expr ctx lhs in
    let rt = infer_expr ctx rhs in
    ignore lt; ignore rt;
    (match op with
     | Add | Sub | Mul | Div -> SymPrim TI32
     | Eq | Neq | Lt | Gt | Le | Ge | And | Or -> SymPrim TBool)
  | EUnary (op, e) ->
    ignore (infer_expr ctx e);
    (match op with Neg -> SymPrim TI32 | Not -> SymPrim TBool)
  | ECall (name, args) ->
    List.iter (fun a -> ignore (infer_expr ctx a)) args;
    (match Symbols.lookup ctx.symbols name with
     | Some (SymFunc (_, ret)) -> ret
     | _ -> SymPrim TI32)
  | ECast (t, e) ->
    ignore (infer_expr ctx e);
    ast_type_to_sym t
