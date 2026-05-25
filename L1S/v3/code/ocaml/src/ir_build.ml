type builder = {
  source: Span.source_file;
  diags: Diagnostic.t list;
}

let make source = { source; diags = [] }

let ast_type_to_ir = function
  | Ast.Prim Ast.I32 -> Ir.I32 | Ast.Prim Ast.I64 -> Ir.I64
  | Ast.Prim Ast.U32 -> Ir.U32 | Ast.Prim Ast.U64 -> Ir.U64
  | Ast.Prim Ast.I8 -> Ir.I8 | Ast.Prim Ast.U8 -> Ir.U8
  | Ast.Prim Ast.Bool -> Ir.Bool | Ast.Prim Ast.Void -> Ir.Void
  | Ast.Ptr _ -> Ir.Ptr Ir.I32
  | Ast.Named _ -> Ir.I32
  | Ast.Array _ -> Ir.Ptr Ir.I32

let build_func func =
  let ret = ast_type_to_ir func.Ast.return_type in
  let ir_func = Ir.make_function func.Ast.name ret in
  ir_func.Ir.params <- List.map (fun p -> ast_type_to_ir p.Ast.type_) func.Ast.params;
  let entry = Ir.make_block "b1" in
  let vars = Hashtbl.create 16 in
  (* Allocate stack slots for parameters *)
  List.iter (fun p ->
    let v = Ir.fresh_vreg ir_func in
    Ir.(entry.instrs <- Alloc (v, ast_type_to_ir p.Ast.type_) :: entry.instrs);
    let pt = ast_type_to_ir p.Ast.type_ in
    let pv = Ir.fresh_vreg ir_func in
    Ir.(entry.instrs <- Const (pv, 0L, pt) :: entry.instrs);
    Ir.(entry.instrs <- Store (v, pv, pt) :: entry.instrs);
    Hashtbl.add vars p.Ast.name (v, pt)
  ) func.Ast.params;
  (* Build body *)
  ignore (build_block ir_func entry func.Ast.body vars);
  ir_func.Ir.blocks <- [entry];
  ir_func

and build_block ir_func block vars =
  let result_instrs = ref [] in
  let result_term = ref (Ir.Ret 0) in
  List.iter (fun stmt ->
    match stmt with
    | Ast.Return expr ->
      let v, _ = build_expr ir_func block vars expr in
      result_term := Ir.Ret v
    | Ast.VarDecl vd ->
      let v = Ir.fresh_vreg ir_func in
      let t = ast_type_to_ir vd.Ast.type_ in
      block.Ir.instrs <- Ir.Alloc (v, t) :: block.Ir.instrs;
      (match vd.Ast.init with
       | Some e ->
         let ev, _ = build_expr ir_func block vars e in
         block.Ir.instrs <- Ir.Store (v, ev, t) :: block.Ir.instrs
       | None -> ());
      Hashtbl.add vars vd.Ast.name (v, t)
    | Ast.If { cond; then_block; else_branch; _ } ->
      let cv, _ = build_expr ir_func block vars cond in
      let then_label = "b_then" in
      let else_label = "b_else" in
      let end_label = "b_end" in
      block.Ir.terminator <- Ir.BrCond (cv, then_label, else_label);
      let then_b = Ir.make_block then_label in
      ignore (build_block ir_func then_b then_block vars);
      then_b.Ir.terminator <- Ir.Br end_label;
      let else_b = Ir.make_block else_label in
      (match else_branch with
       | Some s ->
         (match s with
          | Ast.Block b -> ignore (build_block ir_func else_b b vars)
          | Ast.If _ ->
            let sub_ir_func = ir_func in
            let sub = Ir.make_block "b_sub" in
            else_b.Ir.terminator <- Ir.Br "b_sub";
            let _ = build_if ir_func sub s vars in
            sub.Ir.terminator <- Ir.Br end_label;
            ir_func.Ir.blocks <- ir_func.Ir.blocks @ [sub]
          | _ -> ())
       | None -> ());
      else_b.Ir.terminator <- Ir.Br end_label;
      let end_b = Ir.make_block end_label in
      ir_func.Ir.blocks <- ir_func.Ir.blocks @ [then_b; else_b; end_b];
      block := end_b
    | Ast.While { cond; body; _ } ->
      let cond_label = "b_cond" in
      let body_label = "b_body" in
      let end_label = "b_end" in
      block.Ir.terminator <- Ir.Br cond_label;
      let cond_b = Ir.make_block cond_label in
      let cv, _ = build_expr ir_func cond_b vars cond in
      cond_b.Ir.terminator <- Ir.BrCond (cv, body_label, end_label);
      let body_b = Ir.make_block body_label in
      let _ = build_block ir_func body_b body vars in
      body_b.Ir.terminator <- Ir.Br cond_label;
      let end_b = Ir.make_block end_label in
      ir_func.Ir.blocks <- ir_func.Ir.blocks @ [cond_b; body_b; end_b];
      block := end_b
    | Ast.Block b -> ignore (build_block ir_func block b vars)
    | Ast.Expr e -> let _, _ = build_expr ir_func block vars e in ()
  ) block.Ast.stmts;
  block.Ir.instrs <- List.rev !result_instrs;
  ()

and build_block ir_func ir_block ast_block vars =
  List.iter (fun stmt ->
    match stmt with
    | Ast.Return expr ->
      let v, _ = build_expr ir_func ir_block vars expr in
      ir_block.Ir.terminator <- Ir.Ret v
    | Ast.VarDecl vd ->
      let v = Ir.fresh_vreg ir_func in
      let t = ast_type_to_ir vd.Ast.type_ in
      ir_block.Ir.instrs <- Ir.Alloc (v, t) :: ir_block.Ir.instrs;
      (match vd.Ast.init with
       | Some e ->
         let ev, _ = build_expr ir_func ir_block vars e in
         ir_block.Ir.instrs <- Ir.Store (v, ev, t) :: ir_block.Ir.instrs
       | None -> ());
      Hashtbl.add vars vd.Ast.name (v, t)
    | Ast.If _ -> build_if ir_func ir_block stmt vars
    | Ast.While _ -> build_while ir_func ir_block stmt vars
    | Ast.Block b -> build_block ir_func ir_block b vars
    | Ast.Expr e ->
      let _, _ = build_expr ir_func ir_block vars e in ()
  ) ast_block.Ast.stmts

and build_if ir_func block stmt vars =
  match stmt with
  | Ast.If { cond; then_block; else_branch; _ } ->
    let cv, _ = build_expr ir_func block vars cond in
    let then_lab = Printf.sprintf "b_then_%d" (Ir.fresh_vreg ir_func) in
    let else_lab = Printf.sprintf "b_else_%d" (Ir.fresh_vreg ir_func) in
    let end_lab = Printf.sprintf "b_end_%d" (Ir.fresh_vreg ir_func) in
    block.Ir.terminator <- Ir.BrCond (cv, then_lab, else_lab);
    let then_b = Ir.make_block then_lab in
    build_block ir_func then_b then_block vars;
    then_b.Ir.terminator <- Ir.Br end_lab;
    ir_func.Ir.blocks <- ir_func.Ir.blocks @ [then_b];
    (match else_branch with
     | Some s ->
       let else_b = Ir.make_block else_lab in
       (match s with
        | Ast.Block b -> build_block ir_func else_b b vars; else_b.Ir.terminator <- Ir.Br end_lab
        | Ast.If _ -> build_if ir_func else_b s vars; else_b.Ir.terminator <- Ir.Br end_lab
        | _ -> ());
       ir_func.Ir.blocks <- ir_func.Ir.blocks @ [else_b]
     | None ->
       let else_b = Ir.make_block else_lab in
       else_b.Ir.terminator <- Ir.Br end_lab;
       ir_func.Ir.blocks <- ir_func.Ir.blocks @ [else_b]);
    let end_b = Ir.make_block end_lab in
    ir_func.Ir.blocks <- ir_func.Ir.blocks @ [end_b]
  | _ -> ()

and build_while ir_func block stmt vars =
  match stmt with
  | Ast.While { cond; body; _ } ->
    let cond_lab = Printf.sprintf "b_cond_%d" (Ir.fresh_vreg ir_func) in
    let body_lab = Printf.sprintf "b_body_%d" (Ir.fresh_vreg ir_func) in
    let end_lab = Printf.sprintf "b_end_%d" (Ir.fresh_vreg ir_func) in
    block.Ir.terminator <- Ir.Br cond_lab;
    let cond_b = Ir.make_block cond_lab in
    let cv, _ = build_expr ir_func cond_b vars cond in
    cond_b.Ir.terminator <- Ir.BrCond (cv, body_lab, end_lab);
    ir_func.Ir.blocks <- ir_func.Ir.blocks @ [cond_b];
    let body_b = Ir.make_block body_lab in
    build_block ir_func body_b body vars;
    body_b.Ir.terminator <- Ir.Br cond_lab;
    ir_func.Ir.blocks <- ir_func.Ir.blocks @ [body_b];
    let end_b = Ir.make_block end_lab in
    ir_func.Ir.blocks <- ir_func.Ir.blocks @ [end_b]
  | _ -> ()

and build_expr ir_func block vars expr =
  match expr with
  | Ast.Int n ->
    let v = Ir.fresh_vreg ir_func in
    block.Ir.instrs <- Ir.Const (v, n, Ir.I32) :: block.Ir.instrs;
    (v, Ir.I32)
  | Ast.Bool b ->
    let v = Ir.fresh_vreg ir_func in
    block.Ir.instrs <- Ir.Const (v, if b then 1L else 0L, Ir.Bool) :: block.Ir.instrs;
    (v, Ir.Bool)
  | Ast.Ident name ->
    (match Hashtbl.find_opt vars name with
     | Some (v, t) ->
       let r = Ir.fresh_vreg ir_func in
       block.Ir.instrs <- Ir.Load (r, v, t) :: block.Ir.instrs;
       (r, t)
     | None ->
       let v = Ir.fresh_vreg ir_func in
       block.Ir.instrs <- Ir.Const (v, 0L, Ir.I32) :: block.Ir.instrs;
       (v, Ir.I32))
  | Ast.Binary (op, l, r) ->
    let lv, lt = build_expr ir_func block vars l in
    let rv, _ = build_expr ir_func block vars r in
    let v = Ir.fresh_vreg ir_func in
    let instr = match op with
      | Ast.Add -> Ir.Add (v, lv, rv) | Ast.Sub -> Ir.Sub (v, lv, rv)
      | Ast.Mul -> Ir.Mul (v, lv, rv) | Ast.Div -> Ir.Div (v, lv, rv)
      | Ast.Eq -> Ir.Eq (v, lv, rv) | Ast.Neq ->
        let tmp = Ir.fresh_vreg ir_func in
        block.Ir.instrs <- Ir.Eq (tmp, lv, rv) :: block.Ir.instrs;
        Ir.Not (v, tmp)
      | Ast.Lt -> Ir.Lt (v, lv, rv) | Ast.Gt ->
        let tmp = Ir.fresh_vreg ir_func in
        block.Ir.instrs <- Ir.Lt (tmp, rv, lv) :: block.Ir.instrs;
        Ir.Const (v, 0L, Ir.I32)
      | Ast.Le ->
        let tmp1 = Ir.fresh_vreg ir_func in
        let tmp2 = Ir.fresh_vreg ir_func in
        block.Ir.instrs <- Ir.Lt (tmp1, rv, lv) :: block.Ir.instrs;
        Ir.Not (v, tmp1)
      | Ast.Ge ->
        let tmp = Ir.fresh_vreg ir_func in
        block.Ir.instrs <- Ir.Lt (tmp, lv, rv) :: block.Ir.instrs;
        Ir.Not (v, tmp)
      | Ast.And -> Ir.And (v, lv, rv)
      | Ast.Or -> Ir.Or (v, lv, rv)
    in
    block.Ir.instrs <- instr :: block.Ir.instrs;
    (v, lt)
  | Ast.Unary (op, e) ->
    let ev, et = build_expr ir_func block vars e in
    let v = Ir.fresh_vreg ir_func in
    let instr = match op with
      | Ast.Neg -> Ir.Neg (v, ev)
      | Ast.Not -> Ir.Not (v, ev)
    in
    block.Ir.instrs <- instr :: block.Ir.instrs;
    (v, et)
  | Ast.Call (name, args) ->
    let arg_vs = List.map (fun a -> fst (build_expr ir_func block vars a)) args in
    let v = Ir.fresh_vreg ir_func in
    block.Ir.instrs <- Ir.Call (v, name, arg_vs, Ir.I32) :: block.Ir.instrs;
    (v, Ir.I32)
  | Ast.Cast (t, e) ->
    let ev, _ = build_expr ir_func block vars e in
    (ev, ast_type_to_ir t)
  | Ast.Assign (l, r) ->
    let rv, rt = build_expr ir_func block vars r in
    (match l with
     | Ast.Ident name ->
       (match Hashtbl.find_opt vars name with
        | Some (v, t) ->
          block.Ir.instrs <- Ir.Store (v, rv, t) :: block.Ir.instrs;
          (rv, rt)
        | None -> (rv, rt))
     | _ -> (rv, rt))

let build_module source prog =
  let ir_funcs = List.filter_map (fun decl ->
    match decl with Ast.Func f -> Some (build_func f)) prog.Ast.decls
  in
  { Ir.funcs = ir_funcs; globals = [] }
