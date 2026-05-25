let read_file filename =
  let ch = open_in filename in
  let len = in_channel_length ch in
  let content = really_input_string ch len in
  close_in ch;
  content

let write_file filename content =
  let ch = open_out filename in
  output_string ch content;
  close_in ch

let () =
  let args = Sys.argv in
  if Array.length args < 2 then begin
    Printf.eprintf "usage: %s <input.is> [-o <output.asm>]\n%!" args.(0);
    exit 1
  end;
  let source_file = args.(1) in
  let output_file =
    if Array.length args >= 4 && args.(2) = "-o" then args.(3)
    else Filename.remove_extension source_file ^ ".asm"
  in
  let source = read_file source_file in
  let sf = Span.make_source_file source_file source in
  (* Lex *)
  let toks, lex_diags = Lexer.tokenize source in
  if lex_diags <> [] then begin
    Diagnostic.emit sf lex_diags;
    exit 1
  end;
  (* Parse *)
  let prog = Parser.parse_program toks in
  (* Type check *)
  let tc_diags = Typeck.check_program sf prog in
  if tc_diags <> [] then begin
    Diagnostic.emit sf tc_diags;
    exit 1
  end;
  (* Build IR *)
  let ir_mod = Ir_build.build_module sf prog in
  (* Register allocate *)
  List.iter Regalloc.allocate_function ir_mod.Ir.funcs;
  (* Codegen *)
  let asm = Codegen.generate_module ir_mod in
  write_file output_file asm;
  Printf.printf "wrote %s\n%!" output_file
