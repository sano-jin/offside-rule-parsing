(* main.ml *)

let () =
  print_string @@ Syntax.string_of_stmt @@ Parsing.read_and_parse
  @@ Util.read_file Sys.argv.(1)
