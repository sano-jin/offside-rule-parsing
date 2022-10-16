(* Parser *)

%{
  open Syntax
%}

%token <string> VAR       (* x, y, abc, ... *)
%token <string> STRING    (* 'str', ... *)
%token <int> INT          (* 0, 1, 2, ...  *)

(* operators *)
%token PLUS            (* '+' *)
%token ASTERISK        (* '*' *)
%token LT              (* '<' *)
%token EQEQ            (* "=="  *)
%token EQ              (* '=' *)
%token DELIMITER       (* ';' *)
%token COLON           (* ';' *)
%token COMMA           (* ',' *)

(* Parentheses *)
%token LPAREN          (* '(' *)
%token RPAREN          (* ')' *)

(* Indentation *)
%token INDENT
%token DEDENT
%token <token list> TOKENS    (* Zero or more TOKENs *)


(* reserved names *)
%token TRUE        (* "true"   *)
%token FALSE       (* "false"  *)
%token WHILE       (* "while"  *)
%token PASS        (* "pass"  *)
%token IF          (* "if"  *)
%token ELIF        (* "elif"  *)
%token ELSE        (* "else"  *)
%token NOT         (* "not"  *)
%token DEF         (* "def"    *)
%token RETURN      (* "return" *)

(* End of file *)
%token EOF

(* Operator associativity *)
%nonassoc LT EQEQ 
%left PLUS
%left ASTERISK
%nonassoc LPAREN


%start main
%type <Syntax.block> main

%%

(* Main part must end with EOF (End Of File) *)
main:
  | INDENT block DEDENT EOF { $2 }
  | block EOF { $1 }
  | TOKENS { failwith @@ "error" }


(* arguments *)
args:
  | LPAREN separated_list(COMMA, exp) RPAREN { $2 }


(* vars *)
vars:
  | LPAREN separated_list(COMMA, VAR) RPAREN { $2 }


(* unary expression *)
unary_exp:
  | VAR
    { Var $1 }

  | INT
    { IntLit $1 }

  | TRUE
    { BoolLit true }

  | FALSE
    { BoolLit false }

  | STRING
    { StringLit $1 }

  (* Parentheses *)
  | LPAREN exp RPAREN
    { $2 }


(* expression *)
exp:
  | unary_exp
    { $1 }

  (* Unary not  *)
  | NOT unary_exp
    { Not $2 }

  (* e1 + e2 *)
  | exp PLUS exp
    { Plus ($1, $3) }

  (* e1 * e2 *)
  | exp ASTERISK exp
    { Times ($1, $3) }

  (* e1 < e2 *)
  | exp LT exp
    { Lt ($1, $3) }

  (* e1 == e2 *)
  | exp EQEQ exp
    { Eq ($1, $3) }

  (* application *)
  (* f (e1, ..., en) *)
  | exp args { App ($1, $2) }


(* one line statement *)
stmt_without_delimiter:
  (* f (e1, ..., en) ; *)
  | exp { Exp $1 }

  (* Return *)
  | RETURN exp
    { Return $2 }

  (* Pass *)
  | PASS
    { Skip }

  (* Assignment *)
  | VAR EQ exp
    { Assign ($1, $3) }


(* block *)
stmt:
  (* def f (x1, ..., xn): { block } *)
  | DEF VAR vars COLON INDENT block DEDENT
    { Def ($2, $3, $6) }

  (* while exp block *)
  | WHILE exp COLON INDENT block DEDENT
   { While ($2, $5) }

  | stmt_without_delimiter DELIMITER { $1 }

  | if_elifs_else { $1 }


block:
  | list(stmt) {$1}
  

if_elifs_else:
  (* if elif *)
  | IF exp COLON INDENT block DEDENT elifs
   { If ($2, $5, [$7]) }

  (* if else *)
  | IF exp COLON INDENT block DEDENT else_
   { If ($2, $5, $7) }

  (* if *)
  | IF exp COLON INDENT block DEDENT
   { If ($2, $5, []) }
;


elifs:
  (* elif elif *)
  | ELIF exp COLON INDENT block DEDENT elifs
   { If ($2, $5, [$7]) }

  (* elif else *)
  | ELIF exp COLON INDENT block DEDENT else_
   { If ($2, $5, $7) }

  (* elif *)
  | ELIF exp COLON INDENT block DEDENT
   { If ($2, $5, []) }


else_:
  (* else: block *)
  | ELSE COLON INDENT block DEDENT
   { $4 }

