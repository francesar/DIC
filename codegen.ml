module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)

let translate (globals, functions) =
  let context    = L.global_context () in
  (* Add types to the context so we can use them in our LLVM code *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and void_t     = L.void_type   context 
  and string_t   = L.string_type context (* possibly very wrong*)
  and char_t     = L.char_type   context (* possibly very wrong*)

  and the_module = L.create_module context "DIC" in

  let ltype_of_typ = function
  	| A.Int    -> i32_t
	| A.Bool   -> i1_t
	| A.Float  -> float_t
	| A.Void   -> void_t
	| A.String -> string_t
	| A.Char   -> char_t
  in
  let vars : L.llvalue StringMap.t =
    let vars m (t, n) = 
      let init = match t with
          A.Float -> L.const_float (ltype_of_typ t) 0.0
        | _ -> L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in