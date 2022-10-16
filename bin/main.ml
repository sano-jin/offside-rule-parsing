(* main.ml *)

let () =
  print_endline @@ Syntax.string_of_block @@ Parsing.read_and_parse Sys.argv.(1)
