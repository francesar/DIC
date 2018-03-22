let () =
  let usage_msg = "usage: ./microc.native [file.mc]" in
  let channel = ref stdin in
  Arg.parse [] (fun file -> channel := open_in file) usage_msg;
  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in
  (* let sast = Semant.check ast in 
  print_string (Sast.string_of_sprogram sast) *) 
  let p = Ast.string_of_program ast in print_string p