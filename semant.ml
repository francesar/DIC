open Ast 
open Sast
module StringMap = Map.Make(String)

type func_symbol = func_decl StringMap.t

(* args here might need to change since we accept classes OR statment blocks as valid programs *)
let check (class, functions) = 

  (* FUNCTIONS *)
  let built_in_decls = 
    let add_bind = map (ty, name) = StringMap.add name {
      typ = Void;
      fname = name;
      formals = [(ty, "x")];
      locals = []; 
      body = []
    } map 
    (* Add built in function declarations into arr here *)
    in List.fold_left add_bind StringMap.empty [(Int, "print");]

  (* adding functions to symbol table *)
  let add_func map fd = 
    let built_in_err = "function " ^ fd.name ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.name 
    and make_err err = raise (Failure err) 
    and n = fd.name 
    in match fd with 
      | _ when StringMap.mem n built_in_decls -> make_err built_in_err 
      | _ when StringMap.mem n map -> make_err dup_err
      | _ -> StringMap.add n fd map
  in
  
  (* Add functions to function symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions 
  in 

  (* Finds and returns functions in function symbol table *)
  let find_func s = 
    try StringMap.find s function_decls
    with Not_Found -> raise (Failure ("unrecognized function " ^ s))
  in 

  (* Make sure main function is defined *)
  let _ = find_func "main" 
  in 

  let check_function func = 
    let formals' = check_binds "formal" func.formals in 
    let locals' = check_binds "local" func.locals in 
