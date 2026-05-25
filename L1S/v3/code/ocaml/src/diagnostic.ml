type severity = Error | Warning

type t = {
  span: Span.span;
  message: string;
  severity: severity;
}

let error span msg = { span; message = msg; severity = Error }
let warn span msg = { span; message = msg; severity = Warning }

let emit sf diagnostics =
  List.iter (fun d ->
    let level = match d.severity with Error -> "error" | Warning -> "warning" in
    Printf.eprintf "%s: %s\n" level d.message;
    try
      let underline = Span.format_underline sf d.span in
      Printf.eprintf "%s\n" underline
    with _ -> ()
  ) diagnostics
