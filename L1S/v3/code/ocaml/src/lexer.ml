type lexer = {
  chars: char array;
  mutable pos: int;
  source: Span.source_file;
  diags: Diagnostic.t list;
}

let make chars source = { chars; pos = 0; source; diags = [] }

let is_at_end l = l.pos >= Array.length l.chars
let peek l = if is_at_end l then '\000' else l.chars.(l.pos)
let advance l = let c = peek l in l.pos <- l.pos + 1; c

let skip_whitespace l =
  while not (is_at_end l) && (peek l = ' ' || peek l = '\t' || peek l = '\n' || peek l = '\r') do
    l.pos <- l.pos + 1
  done

let skip_comment l =
  if peek l = '/' && l.pos + 1 < Array.length l.chars && l.chars.(l.pos + 1) = '/' then begin
    l.pos <- l.pos + 2;
    while not (is_at_end l) && peek l <> '\n' do l.pos <- l.pos + 1 done
  end

let lex_word l start =
  while not (is_at_end l) && (let c = peek l in c = '_' || Char.lowercase_ascii c >= 'a' && Char.lowercase_ascii c <= 'z' || c >= '0' && c <= '9') do
    l.pos <- l.pos + 1
  done;
  let word = String.sub (String.of_seq (Array.to_seq l.chars)) start (l.pos - start) in
  match Token.keyword_token word with
  | Some tok -> tok
  | None -> Token.Ident word

let lex_number l start =
  while not (is_at_end l) && (let c = peek l in c >= '0' && c <= '9') do
    l.pos <- l.pos + 1
  done;
  let num = String.sub (String.of_seq (Array.to_seq l.chars)) start (l.pos - start) in
  Token.NumLiteral num

let rec next_token l =
  skip_whitespace l;
  if is_at_end l then Token.Eof
  else begin
    let start = l.pos in
    let c = advance l in
    if c = '/' then begin
      if not (is_at_end l) && peek l = '/' then begin skip_comment l; next_token l end
      else Token.Slash
    end
    else if c = '_' || Char.lowercase_ascii c >= 'a' && Char.lowercase_ascii c <= 'z' then begin
      l.pos <- l.pos - 1; lex_word l start
    end
    else if c >= '0' && c <= '9' then begin
      l.pos <- l.pos - 1; lex_number l start
    end
    else match c with
    | '+' -> Token.Plus | '-' -> Token.Minus | '*' -> Token.Star
    | '(' -> Token.OpenParen | ')' -> Token.CloseParen
    | '{' -> Token.OpenBrace | '}' -> Token.CloseBrace
    | '[' -> Token.OpenBracket | ']' -> Token.CloseBracket
    | ':' -> Token.Colon | ',' -> Token.Comma | ';' -> Token.Semicolon
    | '@' -> Token.At
    | '=' -> if peek l = '=' then begin l.pos <- l.pos + 1; Token.EqEq end else Token.Eq
    | '!' -> if peek l = '=' then begin l.pos <- l.pos + 1; Token.BangEq end else Token.Bang
    | '<' -> if peek l = '=' then begin l.pos <- l.pos + 1; Token.LtEq end else Token.Lt
    | '>' -> if peek l = '=' then begin l.pos <- l.pos + 1; Token.GtEq end else Token.Gt
    | '&' -> if peek l = '&' then begin l.pos <- l.pos + 1; Token.AndAnd end else Token.Eof
    | '|' -> if peek l = '|' then begin l.pos <- l.pos + 1; Token.PipePipe end else Token.Eof
    | _ -> Token.Eof
  end

let tokenize source =
  let chars = Array.of_seq (String.to_seq source) in
  let sf = Span.make_source_file "(source)" source in
  let lex = make chars sf in
  let rec loop acc =
    let start = lex.pos in
    let tok = next_token lex in
    let sp = Span.make start lex.pos in
    let acc' = (tok, sp) :: acc in
    if tok = Token.Eof then List.rev acc' else loop acc'
  in
  let toks = loop [] in
  (toks, lex.diags)
