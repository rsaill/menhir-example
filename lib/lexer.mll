{
open Lexing
open Grammar

exception Error of position * string

let keywords = Hashtbl.create 11

let _ = List.iter (fun (name, keyword) ->
    Hashtbl.add keywords name keyword) [
    "fun",       FUN;
    "var",       VAR;
    "if",        IF;
    "else",      ELSE;
    "while",     WHILE;
    "new",       NEW;
    "return",    RETURN;
  ]

let ident_to_token id =
  try Hashtbl.find keywords id
  with Not_found -> ID id

let rm_1st (x:string) : string =
  String.sub x 1 (String.length x -1)
}

let space   = [' ' '\t' '\r']
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let identifier = letter ( ( letter | digit | '_') )*
let qmark = '?' letter ( ( letter | digit | '_') )*

let numeral = digit ('_'? digit)*

rule token = parse
  | space       { token lexbuf  }
  | '\n'        { new_line lexbuf ; token lexbuf }
  | "//"        { comment lexbuf}

  | '('         { LPAR  }
  | ')'         { RPAR  }
  | '{'         { LBRA  }
  | '}'         { RBRA  }
  | ';'         { SEMICOLON }
  | ','         { COMMA }

  | '*'         { STAR }
  | '+'         { PLUS }
  | '-'         { MINUS }
  | '/'         { DIV }
  | '%'         { MOD }

  | '#'         { SHARP }

  | '&'         { AND }
  | '|'         { OR }
  | '!'         { NOT }
  | "=>"        { IMP }

  | '<'         { LT }
  | '>'         { GT }
  | ">="        { GE }
  | "<="        { LE }

  | ":="        { ASSIGN }
  | "=="        { EQUAL }
  | "!="        { NOT_EQUAL }

  | identifier as id { ident_to_token id  }
  | numeral as i { INTEGER (int_of_string i) }
  | _   as c    { raise (Error (lexbuf.Lexing.lex_start_p, "unexpected character '" ^ String.make 1 c ^ "'.")) }
  | eof { EOF }

 and comment = parse
  | '\n' { new_line lexbuf ; token lexbuf }
  | _    { comment lexbuf        }
  | eof	 { raise (Error (lexbuf.Lexing.lex_start_p, "unexpected end of file." )) }
