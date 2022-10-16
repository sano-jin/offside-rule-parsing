(* Lexer *)

{
  open Parser
  open Lexing_aux
}

let space = [' ' '\t']
let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z' '_']
let alnum = digit | alpha | '\''
let newline = '\r' | '\n' | "\r\n"

(* 改行後のスペースを indent で読んだ後に呼ばれる Lexer *)
rule token = parse
  (* Number *)
  | digit+
    { let str = Lexing.lexeme lexbuf in
      INT (int_of_string str) }

  (* Operators *)
  | '+'               { PLUS }
  | '*'               { ASTERISK }
  | '<'               { LT }
  | '='               { EQ }
  | "=="              { EQEQ }
  | ","               { COMMA }
  | ":"               { COLON }

  (* Parentheses *)
  | '('               { LPAREN }
  | ')'               { RPAREN }

  (* reserved names *)
  | "true"            { TRUE }
  | "false"           { FALSE }
  | "while"           { WHILE }
  | "not"             { NOT }
  | "if"              { IF }
  | "elif"            { ELIF }
  | "else"            { ELSE }
  | "def"             { DEF }
  | "return"          { RETURN }

  (* variable *)
  | alpha alnum*
    { VAR (Lexing.lexeme lexbuf) }

  (* end of file *)
  | eof
    { EOF }

  (* spaces *)
  | space+
    { token lexbuf }

  (* new line. call the [indent] tokenizer *)
  | newline
    { Lexing.new_line lexbuf; indent lexbuf }

  (* comments *)
  | '#' [^ '\n']*
    { token lexbuf }

  (* string *)
  | ''' [^ '\'']* '\''
    { let str = Lexing.lexeme lexbuf in
      STRING (String.sub str 1 @@ String.length str - 2)
    }

  | _
    {
      let message = Printf.sprintf
        "Unknown token '%s' near line %d (near characters %d-%d)"
        (Lexing.lexeme lexbuf)
        (pred lexbuf.lex_curr_p.pos_lnum)
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf)
      in
      raise @@ SyntaxError message
    }


(* 改行があった場合に直後に呼ばれる Lexer *)
and indent = parse
  (* blank line (with a comment) *)
  | space* ( newline | '#' [^ '\n']* newline )
    { Lexing.new_line lexbuf; indent lexbuf }

  (* indent (assuming that the next comming token is
     not a space/newline/comment) *)
  | space*
    { let indent_level =
        String.length @@ Lexing.lexeme lexbuf
      in
      Lexing_aux.emit_indent indent_level
    }

