(** syntax.ml *)

open Util

(** expression *)
type exp =
  | Var of string  (** variable e.g. [x] *)
  | IntLit of int  (** integer literal e.g. [17] *)
  | BoolLit of bool  (** boolean literal e.g. [true] *)
  | StringLit of string  (** string literal e.g. ["dog"] *)
  | Plus of exp * exp  (** [e + e] *)
  | Times of exp * exp  (** [e * e] *)
  | Lt of exp * exp  (** [e < e] *)
  | Eq of exp * exp  (** [e = e] *)
  | Not of exp  (** [not e] *)
  | App of exp * exp list  (** [f (x1, ..., xn)] *)

type stmt =
  | Exp of exp
  | Def of string * string list * block
      (** definition e.g. [def succ(x): { return x + 1 }] *)
  | Assign of string * exp  (** assignment e.g. [x := 1 + 2 * y] *)
  (*
  | Seq of block * block  (** sequence e.g. [x := 2; y := x + 1] *)
  *)
  | While of exp * block  (** loop e.g. [while 1 < x: { x := x + 1 }] *)
  | If of exp * block * block  (** branch e.g. [if 1 < x: { x := x + 1 }] *)
  | Return of exp  (** [return x + 1] *)
  | Skip  (** skip. e.g. [pass] *)

and block = stmt list

let rec string_of_exp = function
  | Var str -> str
  | IntLit i -> string_of_int i
  | BoolLit b -> string_of_bool b
  | StringLit str -> "\"" ^ String.escaped str ^ "\""
  | Plus (e1, e2) -> "(" ^ string_of_exp e1 ^ " + " ^ string_of_exp e2 ^ ")"
  | Times (e1, e2) -> "(" ^ string_of_exp e1 ^ " * " ^ string_of_exp e2 ^ ")"
  | Lt (e1, e2) -> "(" ^ string_of_exp e1 ^ " < " ^ string_of_exp e2 ^ ")"
  | Eq (e1, e2) -> "(" ^ string_of_exp e1 ^ " == " ^ string_of_exp e2 ^ ")"
  | Not e -> "(not " ^ string_of_exp e ^ ")"
  | App (e, args) ->
      string_of_exp e ^ "("
      ^ String.concat ", " (List.map string_of_exp args)
      ^ ")"

let rec string_of_stmt indent =
  let str_of block =
    String.concat "\n" @@ List.map (string_of_stmt ("  " ^ indent)) block
  in
  ( ^ ) indent <. function
  | Exp e -> string_of_exp e
  | Def (f, args, stmt) ->
      "def " ^ f ^ " (" ^ String.concat ", " args ^ "):\n" ^ str_of stmt
  | Assign (x, e) -> x ^ " = " ^ string_of_exp e
  (*
  | Seq (s1, s2) -> string_of_stmt indent s1 ^ string_of_stmt indent s2
 *)
  | While (e, stmt) -> "while " ^ string_of_exp e ^ ":\n" ^ str_of stmt
  | If (e, s1, []) -> "if " ^ string_of_exp e ^ ":\n" ^ str_of s1
  | If (e, s1, s2) ->
      "if " ^ string_of_exp e ^ ":\n" ^ str_of s1 ^ "\n" ^ indent ^ "else:\n"
      ^ str_of s2
  | Return e -> "return " ^ string_of_exp e
  | Skip -> "pass"

let string_of_block = String.concat "\n" <. List.map @@ string_of_stmt ""
