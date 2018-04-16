let () =
  let usage_msg = "usage: ./microc.native [file.mc]" in
  let channel = ref stdin in
  Arg.parse [] (fun file -> channel := open_in file) usage_msg;
  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in
<<<<<<< HEAD
  let sast = Semant.check ast in
  (* let lir = Llvm.string_of_llmodule (Codegen.translate sast) in *)
  (* print_string (Ast.string_of_program ast) *)
  print_string (Sast.string_of_sprogram sast)
=======
  let sast = Semant.check ast in print_string (Sast.string_of_sprogram sast)
  (* let lir = Llvm.string_of_llmodule (Codegen.translate sast) in
  print_string (lir) *)
>>>>>>> 12e324fe5c4295f986fc9596a8edc82d5a0fdaf9
