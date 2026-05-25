let () =
  let args = Sys.argv |> Array.to_list in
  if List.length args < 2 then begin
    Printf.eprintf "Usage: l1s <input.is> [-o <output.asm>]\n";
    exit 1
  end;

  let input_file = List.nth args 1 in
  let output_file =
    if List.length args > 3 && List.nth args 2 = "-o" then
      List.nth args 3
    else begin
      let base = try
        let dot = String.rindex input_file '.'
        in String.sub input_file 0 dot
      with Not_found -> input_file
      in
      base ^ ".asm"
    end
  in

  let source = try
    let ch = open_in input_file in
    let len = in_channel_length ch in
    let s = really_input_string ch len in
    close_in ch;
    s
  with e ->
    Printf.eprintf "error reading '%s': %s\n" input_file (Printexc.to_string e);
    exit 1
  in

  let diags = Diag.create () in

  (* Phase 1: Lexing & Parsing *)
  let parser = Parser.create source diags in
  let program = Parser.parse_program parser in

  if Diag.has_errors diags then begin
    Diag.emit diags source;
    exit 1
  end;

  (* Phase 2: Type checking *)
  let typeck = Typeck.create diags in
  Typeck.check_prog typeck program;

  if Diag.has_errors diags then begin
    Diag.emit diags source;
    exit 1
  end;

  (* Phase 3: IR building *)
  let ir_builder = Ir_build.create diags in
  Ir_build.build ir_builder program;
  let ir_module: Ir.ir_module = { funcs = ir_builder.module_funcs } in

  if Diag.has_errors diags then begin
    Diag.emit diags source;
    exit 1
  end;

  (* Phase 4: IR optimization *)
  let opt_ctx = Ir_opt.create () in
  let optimized_funcs = Ir_opt.run_all opt_ctx ir_module.funcs in
  let ir_module = { ir_module with funcs = optimized_funcs } in

  (* Phase 5: Register allocation *)
  let allocs = List.map (fun func ->
    let alloc = Regalloc.allocate func in
    (func.f_name, alloc)
  ) ir_module.funcs in

  (* Phase 6: Code generation *)
  let cg = Codegen.create () in
  let asm = Codegen.generate cg ir_module allocs in

  (try
    let oc = open_out output_file in
    output_string oc asm;
    close_out oc;
    Printf.eprintf "wrote %s\n" output_file
  with e ->
    Printf.eprintf "error writing '%s': %s\n" output_file (Printexc.to_string e);
    exit 1)
