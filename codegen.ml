module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)

let translate (_, _, functions) =
  let context    = L.global_context () in
  (* Add types to the context so we can use them in our LLVM code *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and string_t   = L.i8_type     context (* possibly very wrong*)
  and void_t     = L.void_type   context 

  and the_module = L.create_module context "DIC" in

  let ltype_of_typ = function 
    | A.Int -> i32_t
    | A.String -> i8_t 
    | A.Void  -> void_t
    | t -> raise (Failure ("Type " ^ A.string_of_typ t ^ " not implemented yet"))
  in 

  let printf_t : L.lltype = 
    L.var_arg_function_type string_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
    L.declare_function "printf" printf_t the_module in  

  let to_imp str = raise (Failure ("Not yet implemented: " ^ str)) in

  let build_function fdecl =

    (* int main() {}  ----->  define i32 @main() {}  *)
    let main_ty = L.function_type (ltype_of_typ fdecl.styp) [||] in
    let the_function = L.define_function "main" main_ty the_module in

    (* An LLVM "instruction builder" points to a basic block. 
     * Adding a new instruction mutates both the_module and builder. *) 
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let string_format_str = L.build_global_stringptr "%s\n" "fmt" builder in

    let rec expr builder ((_, e) : sexpr) = match e with 
      | SLit i -> L.const_int i32_t i
      | SStringLit s -> L.build_global_stringptr s "tmp" builder 
      | SCall("printstr", [e]) -> 
        L.build_call printf_func [| string_format_str; (expr builder e) |] "printf" builder
      | _ -> to_imp (string_of_sexpr (A.Int, e))
    in 
  
    let rec stmt builder = function
      | SExpr e -> let _ = expr builder e in builder 
      | SBlock sl -> List.fold_left stmt builder sl
      (* return 0;  ----->  ret i32 0 *)
      | SReturn e -> let _ = match fdecl.styp with
                              A.Int -> L.build_ret (expr builder e) builder 
                            | _ -> to_imp (A.string_of_typ fdecl.styp)
                     in builder
      | s -> to_imp (string_of_sstmt s)
      in ignore (stmt builder (SBlock fdecl.sbody))

      in List.iter build_function functions; the_module
