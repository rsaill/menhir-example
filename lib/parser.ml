module G = Grammar
module L = Lexer
module S = Syntax

module I = G.MenhirInterpreter
module Inc = G.Incremental

let token_to_string (tk:G.token) : string =
  match tk with
  | WHILE -> "while"
  | VAR -> "var"
  | STAR -> "*"
  | SHARP -> "#"
  | SEMICOLON -> ";"
  | RPAR -> ")"
  | RETURN -> "return"
  | RBRA -> "}"
  | PLUS -> "+"
  | OR -> "|"
  | NOT_EQUAL -> "!="
  | NOT -> "!"
  | NEW -> "new"
  | MOD -> "mod"
  | MINUS -> "-"
  | LT -> "<"
  | LPAR -> "("
  | LE -> "<="
  | LBRA -> "{"
  | INTEGER i -> string_of_int i
  | IMP -> "=>"
  | IF -> "if"
  | ID id -> "ID(" ^ id ^ ")"
  | FUN -> "fun"
  | EQUAL -> "=="
  | EOF -> "eof"
  | ELSE -> "else"
  | DIV -> "/"
  | COMMA -> ","
  | ASSIGN -> ":="
  | AND -> "&"
  | GE -> ">="
  | GT -> ">"

let error : type a b. (a, out_channel, unit, b) format4 -> a = fun fmt ->
  Printf.kfprintf (fun _ -> exit 1) stderr fmt

let parser_error loc tk =
  error "[ERROR][PARSER][%a] Synatx error near token '%s'.\n" S.Loc.pp loc (token_to_string tk)

let lexer_error loc tk =
  error "[ERROR][LEXER][%a] %s\n" S.Loc.pp loc tk

let rec loop (last:G.token) (lexbuf:Lexing.lexbuf) (checkpoint : S.prog I.checkpoint) : S.prog =
  match checkpoint with
  | I.InputNeeded _ ->
    let token =
      try L.token lexbuf
      with L.Error (pos,txt) -> lexer_error (S.Loc.from_lexing_pos pos) txt
    in
    let startp = lexbuf.lex_start_p
    and endp = lexbuf.lex_curr_p in
    let checkpoint = I.offer checkpoint (token, startp, endp) in
    loop token lexbuf checkpoint
  | I.Shifting _
  | I.AboutToReduce _ ->
    let checkpoint = I.resume checkpoint in
    loop last lexbuf checkpoint
  | I.HandlingError _ ->
    parser_error (S.Loc.current lexbuf) last
  | I.Accepted v -> v
  | I.Rejected -> assert false

let parse_file (filename:string) : S.prog =
  let input = open_in filename in
  let lexbuf = Lexing.from_channel input in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  loop EOF lexbuf (Inc.prog (Lexing.dummy_pos))
