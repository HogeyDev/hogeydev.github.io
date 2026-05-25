type severity = SError | SWarning | SNote

type diagnostic = {
  sev: severity;
  msg: string;
  span: Span.span option;
}

type t = {
  diags: diagnostic list ref;
}

let create () = { diags = ref [] }

let add d t = t.diags := d :: !(t.diags)

let error t ?span msg = add { sev = SError; msg; span } t

let warn t ?span msg = add { sev = SWarning; msg; span } t

let has_errors t = List.exists (fun d -> d.sev = SError) !(t.diags)

let emit t source_file =
  List.iter (fun d ->
    let prefix = match d.sev with
      | SError -> "error" | SWarning -> "warning" | SNote -> "note"
    in
    let span_str = match d.span with
      | Some s -> Span.to_string s | None -> ""
    in
    Printf.eprintf "%s: %s [%s]\n" prefix d.msg span_str
  ) (List.rev !(t.diags))
