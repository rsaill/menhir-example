type binary_op =
  | Eq | Neq | Ge | Gt | Le | Lt | And | Or | Imp | Plus | Minus
  | Mult | Div | Modulo

let bop_to_string = function
  | Eq -> "=="
  | Neq -> "!="
  | Ge -> ">="
  | Gt -> ">"
  | Le -> "<="
  | Lt -> "<"
  | And -> "&"
  | Or -> "|"
  | Imp -> "=>"
  | Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Modulo -> "%"

module Loc = struct
  type t = { file:string; line:int; column:int }

  let from_lexing_pos (loc:Lexing.position) : t =
    let open Lexing in
    { file=loc.pos_fname;
      line=loc.pos_lnum;
      column=loc.pos_cnum-loc.pos_bol+1 }

  let current (lb:Lexing.lexbuf) : t =
    from_lexing_pos (lb.lex_curr_p)

  let pp (out:out_channel) (loc:t) : unit = 
    Printf.fprintf out "%s:%i:%i" loc.file loc.line loc.column 
end


type 'a node = {
  loc: Loc.t;
  desc: 'a;
}

type term_desc =
  | Id of string
  | Load of term
  | IntLit of int
  | BinExpr of binary_op*term*term
  | Not of term

and term = term_desc node

type assign_expr =
  | A_Expr of term
  | A_New

type stmt_desc =
  | Nop
  | Seq of stmt * stmt
  | Assign of string * assign_expr
  | Store of term * term
  | IfThenElse of term * stmt * stmt
  | VarDecl of string * assign_expr
  | Return
  | While of term * stmt
  | MethodCall of string * term list

and stmt = stmt_desc node

type func = {
  loc: Loc.t;
  name: string;
  body: stmt;
  params: string list;
}

type prog = func list
