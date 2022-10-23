%{
open Syntax

let mk pos desc : _ node =
  {loc=Loc.from_lexing_pos pos ;desc}
%}

%token <string> ID
%token <int> INTEGER

%token LPAR
%token RPAR
%token LBRA
%token RBRA
%token SEMICOLON
%token STAR
%token PLUS
%token MINUS
%token MOD
%token DIV
%token SHARP
%token AND
%token OR
%token IMP
%token NOT
%token LT
%token GT
%token GE
%token LE
%token ASSIGN
%token EQUAL
%token NOT_EQUAL
%token COMMA
%token FUN
%token VAR
%token IF
%token ELSE
%token WHILE
%token NEW
%token RETURN

%token EOF

%start prog
%type <Syntax.prog> prog
%type <Syntax.term> term
%type <Syntax.stmt> stmt
%type <Syntax.func> func

%right SEMICOLON
%left  IMP
%left  OR
%left  AND
%left  EQUAL NOT_EQUAL
%left  LT LE GT GE
%left  PLUS MINUS
%left  STAR DIV MOD
%nonassoc NOT SHARP

%%

%inline binop:
      EQUAL { Eq }
    | NOT_EQUAL { Neq }
    | LE { Le }
    | LT { Lt }
    | GE { Ge }
    | GT { Gt }
    | AND { And }
    | OR { Or }
    | IMP { Imp }
    | PLUS { Plus }
    | MINUS { Minus }
    | STAR { Mult }
    | DIV { Div }
    | MOD { Modulo }

term:
  id=ID { mk $startpos (Id id) }
| SHARP t=term { mk $startpos (Load t) }
| i=INTEGER { mk $startpos (IntLit i) }
| t1=term op=binop t2=term { mk $startpos (BinExpr(op,t1,t2)) }
| NOT t=term { mk $startpos (Not t) }
| LPAR t=term RPAR { t }

stmt: 
  s1=stmt SEMICOLON s2=stmt { mk $startpos (Seq(s1,s2)) }
| v=ID ASSIGN t=term { mk $startpos (Assign(v,A_Expr t)) }
| VAR v=ID ASSIGN t=term { mk $startpos (VarDecl (v,A_Expr t)) }
| v=ID ASSIGN NEW LPAR RPAR { mk $startpos (Assign(v,A_New)) }
| VAR v=ID ASSIGN NEW LPAR RPAR { mk $startpos (VarDecl (v,A_New)) }
| SHARP addr=term ASSIGN v=term { mk $startpos (Store(addr,v)) }
| RETURN { mk $startpos Return }
| f=ID LPAR params=separated_list(COMMA,term) RPAR { mk $startpos (MethodCall(f,params)) }
| IF LPAR cond=term RPAR LBRA s1=stmt RBRA ELSE LBRA s2=stmt RBRA  { mk $startpos (IfThenElse(cond,s1,s2)) }
| IF LPAR cond=term RPAR LBRA s1=stmt RBRA  { mk $startpos (IfThenElse(cond,s1,mk $startpos Nop)) }
| WHILE LPAR cond=term RPAR LBRA body=stmt RBRA { mk $startpos (While(cond,body)) }

param: id=ID { id }

func: FUN name=ID LPAR params=separated_list(COMMA,param) RPAR
          LBRA body=stmt RBRA
          { {loc=Loc.from_lexing_pos $startpos;name;params;body} }

prog: lst=list(func) EOF { lst }

%%

