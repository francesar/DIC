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
  and float_t    = L.double_type context
  and i1_t       = L.i1_type     context
  (* and intM_t     = L.pointer_type int_mat_struct *)
  (* and intM_t      = L.pointer_type (L.pointer_type (L.i32_type context)) *)

  and the_module = L.create_module context "DIC" in

  let int_array_struct = L.named_struct_type context "int_array_struct" in 
    let body = [|  i32_t; L.pointer_type i32_t |] in
    ignore (L.struct_set_body int_array_struct body false);
  
  let int_mat_struct = L.named_struct_type context "int_mat_struct" in 
  let body = [|  i32_t; L.pointer_type(L.pointer_type int_array_struct) |] in
  ignore (L.struct_set_body int_mat_struct body false);

  let float_array_struct = L.named_struct_type context "float_array_struct" in 
    let body = [|  i32_t; L.pointer_type float_t |] in
    ignore (L.struct_set_body float_array_struct body false);
  
  let float_mat_struct = L.named_struct_type context "float_mat_struct" in 
  let body = [|  i32_t; L.pointer_type(L.pointer_type float_array_struct) |] in
  ignore (L.struct_set_body float_mat_struct body false);

  let string_array_struct = L.named_struct_type context "string_array_struct" in 
    let body = [|  i32_t; L.pointer_type string_t |] in
    ignore (L.struct_set_body string_array_struct body false);
  
  let string_mat_struct = L.named_struct_type context "string_mat_struct" in 
  let body = [|  i32_t; L.pointer_type(L.pointer_type string_array_struct) |] in
  ignore (L.struct_set_body string_mat_struct body false);

  let bool_array_struct = L.named_struct_type context "bool_array_struct" in 
    let body = [|  i32_t; L.pointer_type i1_t |] in
    ignore (L.struct_set_body bool_array_struct body false);
  
  let bool_mat_struct = L.named_struct_type context "bool_mat_struct" in 
  let body = [|  i32_t; L.pointer_type(L.pointer_type bool_array_struct) |] in
  ignore (L.struct_set_body bool_mat_struct body false);

  let char_array_struct = L.named_struct_type context "char_array_struct" in 
    let body = [|  i32_t; L.pointer_type i1_t |] in
    ignore (L.struct_set_body char_array_struct body false);
  
  let char_mat_struct = L.named_struct_type context "char_mat_struct" in 
  let body = [|  i32_t; L.pointer_type(L.pointer_type bool_array_struct) |] in
  ignore (L.struct_set_body char_mat_struct body false);


  let ltype_of_typ = function
    | A.Int -> i32_t
    | A.String -> L.pointer_type string_t
    | A.Void  -> void_t
    | A.Bool -> i1_t
    | A.Float -> float_t
    | A.Char -> i1_t
    | A.IntM -> L.pointer_type int_array_struct
    (* | A.IntM -> L.pointer_type i32_t *)
    | A.FloatM -> L.pointer_type float_array_struct
    | A.StringM -> L.pointer_type string_array_struct
    | A.BoolM -> L.pointer_type bool_array_struct
    | A.CharM -> L.pointer_type char_array_struct
    | _ -> raise(Failure("Error"))
    (* | t -> raise (Failure ("Type " ^ A.string_of_typ t ^ " not implemented yet")) *)
  in

  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in

  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in

  (******* CONC FUNCTIONS *******)
  (* let start_t  = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let start_func = L.declare_function "start" start_t the_module in
 *)

  (******* PRINTING FUNCTIONS *******)

  let printf_int = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_intfunc = L.declare_function "printf" printf_int the_module in

  let printf_float = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_float_func = L.declare_function "printf" printf_float the_module in

  let printf_list_int = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_list_int_func = L.declare_function "print_intlist" printf_list_int the_module in

  let printf_list_float = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_list_float_func = L.declare_function "print_floatlist" printf_list_float the_module in
(* 
  let printf_list_string = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_list_string_func = L.declare_function "printlist_string" printf_list the_module in *)

  let printf_mat_int = L.var_arg_function_type i32_t [| L.pointer_type i8_t; i1_t |] in
  let printf_mat_int_func = L.declare_function "print_intmat" printf_mat_int the_module in

  let printf_mat_float = L.var_arg_function_type i32_t [| L.pointer_type i8_t; i1_t |] in
  let printf_mat_float_func = L.declare_function "print_floatmat" printf_mat_float the_module in

  (******* FILE I/O FUNCTIONS *******)
  let write_string_to_file_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t; L.pointer_type i8_t |] in 
  let write_string_to_file_func = L.declare_function "write_string_to_file" write_string_to_file_t the_module in 

  let read_intmat_from_file_t = L.var_arg_function_type (L.pointer_type int_mat_struct) [| L.pointer_type i8_t |] in
  let read_intmat_from_file_func = L.declare_function "read_intmat_from_file" read_intmat_from_file_t the_module in


  (******* LIST FUNCTIONS *******)
  let len_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in 
  let len_func = L.declare_function "len" len_t the_module in 

  let dot_prod_t = L.var_arg_function_type i32_t [| L.pointer_type int_array_struct; L.pointer_type int_array_struct |] in 
  let dot_prod_func = L.declare_function "dot_prod_int" dot_prod_t the_module in 

  let dot_prod_t_float = L.var_arg_function_type float_t [| L.pointer_type float_array_struct; L.pointer_type float_array_struct |] in 
  let dot_prod_func_float = L.declare_function "dot_prod_float" dot_prod_t_float the_module in 

  let elem_mult_list_t = L.var_arg_function_type (L.pointer_type int_array_struct) [| L.pointer_type int_array_struct; L.pointer_type int_array_struct |] in 
  let elem_mult_list_func = L.declare_function "elem_mult_list_int" elem_mult_list_t the_module in 

  let const_mult_list_t = L.var_arg_function_type (L.pointer_type int_array_struct) [| i32_t; L.pointer_type int_array_struct |] in 
  let const_mult_list_func = L.declare_function "const_mult_list_int" const_mult_list_t the_module in 

  let const_mult_list_t_float = L.var_arg_function_type (L.pointer_type float_array_struct) [| float_t; L.pointer_type float_array_struct |] in 
  let const_mult_list_func_float = L.declare_function "const_mult_list_float" const_mult_list_t_float the_module in 

  let const_add_list_t = L.var_arg_function_type (L.pointer_type int_array_struct) [| i32_t; L.pointer_type int_array_struct |] in 
  let const_add_list_func = L.declare_function "const_add_list_int" const_add_list_t the_module in 

  let const_add_list_t_float = L.var_arg_function_type (L.pointer_type float_array_struct) [| float_t; L.pointer_type float_array_struct |] in 
  let const_add_list_func_float = L.declare_function "const_add_list_float" const_add_list_t_float the_module in 

  let elem_mult_list_float_t = L.var_arg_function_type (L.pointer_type float_array_struct) [| L.pointer_type float_array_struct; L.pointer_type float_array_struct |] in 
  let elem_mult_list_func_float = L.declare_function "elem_mult_list_float" elem_mult_list_float_t the_module in 

  let elem_div_list_t = L.var_arg_function_type (L.pointer_type int_array_struct) [| L.pointer_type int_array_struct; L.pointer_type int_array_struct |] in 
  let elem_div_list_func = L.declare_function "elem_div_list_int" elem_div_list_t the_module in 

  let elem_div_list_float_t = L.var_arg_function_type (L.pointer_type float_array_struct) [| L.pointer_type float_array_struct; L.pointer_type float_array_struct |] in 
  let elem_div_list_func_float = L.declare_function "elem_div_list_float" elem_div_list_float_t the_module in 

  let append_t = L.var_arg_function_type (L.pointer_type int_array_struct) [| L.pointer_type i8_t ; L.pointer_type i8_t |] in 
  let append_func = L.declare_function "append" append_t the_module in

  let add_list_t = L.var_arg_function_type (L.pointer_type int_array_struct) [| L.pointer_type int_array_struct; L.pointer_type int_array_struct |] in
  let add_list_func = L.declare_function "add_list_int" add_list_t the_module in

  let sub_list_t = L.var_arg_function_type (L.pointer_type int_array_struct) [| L.pointer_type int_array_struct; L.pointer_type int_array_struct |] in
  let sub_list_func = L.declare_function "sub_list_int" sub_list_t the_module in

  let add_list_t_float = L.var_arg_function_type (L.pointer_type float_array_struct) [| L.pointer_type float_array_struct; L.pointer_type float_array_struct |] in
  let add_list_func_float = L.declare_function "add_list_float" add_list_t_float the_module in

  let sub_list_t_float = L.var_arg_function_type (L.pointer_type float_array_struct) [| L.pointer_type float_array_struct; L.pointer_type float_array_struct |] in
  let sub_list_func_float = L.declare_function "sub_list_float" sub_list_t_float the_module in

  (******* MATRIX FUNCTIONS *******)
  let len_t_mat = L.var_arg_function_type (L.pointer_type int_array_struct) [| L.pointer_type i8_t |] in 
  let len_func_mat = L.declare_function "len_mat" len_t_mat the_module in 

  let len_t_mat_float = L.var_arg_function_type (L.pointer_type int_array_struct) [| L.pointer_type i8_t |] in 
  let len_func_mat_float = L.declare_function "len_mat_float" len_t_mat_float the_module in 
 
  let add_mat_t = L.var_arg_function_type (L.pointer_type int_mat_struct) [| L.pointer_type i8_t; L.pointer_type i8_t |] in
  let add_mat_func = L.declare_function "add_mat_int" add_mat_t the_module in

  let sub_mat_t = L.var_arg_function_type (L.pointer_type int_mat_struct) [| L.pointer_type i8_t; L.pointer_type i8_t |] in
  let sub_mat_func = L.declare_function "sub_mat_int" sub_mat_t the_module in
  
  let elem_mult_mat_t = L.var_arg_function_type (L.pointer_type int_mat_struct) [| L.pointer_type i8_t; L.pointer_type i8_t |] in 
  let elem_mult_mat_func = L.declare_function "elem_mult_mat_int" elem_mult_mat_t the_module in 

  let elem_div_mat_t = L.var_arg_function_type (L.pointer_type int_mat_struct) [| L.pointer_type i8_t; L.pointer_type i8_t |] in 
  let elem_div_mat_func = L.declare_function "elem_div_mat_int" elem_div_mat_t the_module in 

  let const_add_mat_t = L.var_arg_function_type (L.pointer_type int_mat_struct) [| i32_t; L.pointer_type i8_t |] in 
  let const_add_mat_func = L.declare_function "const_add_mat_int" const_add_mat_t the_module in 

  let const_mult_mat_t = L.var_arg_function_type (L.pointer_type int_mat_struct) [| i32_t; L.pointer_type i8_t |] in 
  let const_mult_mat_func = L.declare_function "const_mult_mat_int" const_mult_mat_t the_module in 

  let mult_mat_t = L.var_arg_function_type (L.pointer_type int_mat_struct) [| L.pointer_type i8_t; L.pointer_type i8_t |] in
  let mult_mat_func = L.declare_function "mult_mat_int" mult_mat_t the_module in

  let add_mat_t_float = L.var_arg_function_type (L.pointer_type float_mat_struct) [| L.pointer_type i8_t; L.pointer_type i8_t |] in
  let add_mat_func_float = L.declare_function "add_mat_float" add_mat_t_float the_module in

  let sub_mat_t_float = L.var_arg_function_type (L.pointer_type float_mat_struct) [| L.pointer_type i8_t; L.pointer_type i8_t |] in
  let sub_mat_func_float = L.declare_function "sub_mat_float" sub_mat_t_float the_module in

  let elem_mult_mat_t_float = L.var_arg_function_type (L.pointer_type float_mat_struct) [| L.pointer_type i8_t; L.pointer_type i8_t |] in 
  let elem_mult_mat_func_float = L.declare_function "elem_mult_mat_float" elem_mult_mat_t_float the_module in 

  let elem_div_mat_t_float = L.var_arg_function_type (L.pointer_type float_mat_struct) [| L.pointer_type i8_t; L.pointer_type i8_t |] in 
  let elem_div_mat_func_float = L.declare_function "elem_div_mat_float" elem_div_mat_t_float the_module in 

  let const_add_mat_t_float = L.var_arg_function_type (L.pointer_type float_mat_struct) [| float_t; L.pointer_type i8_t |] in 
  let const_add_mat_func_float = L.declare_function "const_add_mat_float" const_add_mat_t_float the_module in 

  let const_mult_mat_t_float = L.var_arg_function_type (L.pointer_type float_mat_struct) [| float_t; L.pointer_type i8_t |] in 
  let const_mult_mat_func_float = L.declare_function "const_mult_mat_float" const_mult_mat_t_float the_module in 

  let mult_mat_t_float = L.var_arg_function_type (L.pointer_type float_mat_struct) [| L.pointer_type i8_t; L.pointer_type i8_t |] in
  let mult_mat_func_float = L.declare_function "mult_mat_float" mult_mat_t_float the_module in

  let det_mat_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let det_mat_func = L.declare_function "determinant_int" det_mat_t the_module in

  let det_mat_t_float = L.var_arg_function_type float_t [| L.pointer_type i8_t |] in
  let det_mat_func_float = L.declare_function "determinant_float" det_mat_t_float the_module in

  let trans_mat_t = L.var_arg_function_type (L.pointer_type int_mat_struct) [| L.pointer_type i8_t |] in
  let trans_mat_func = L.declare_function "transpose_int" trans_mat_t the_module in

  let trans_mat_t_float = L.var_arg_function_type (L.pointer_type float_mat_struct) [| L.pointer_type i8_t |] in
  let trans_mat_func_float = L.declare_function "transpose_float" trans_mat_t_float the_module in

  let is_square_t = L.var_arg_function_type i1_t [| L.pointer_type int_mat_struct |] in 
  let is_square_func = L.declare_function "is_square" is_square_t the_module in 

  let fmat_fromcsv_t = L.var_arg_function_type (L.pointer_type float_mat_struct) [| L.pointer_type i8_t |] in 
  let fmat_csv = L.declare_function "fmat_fromcsv" fmat_fromcsv_t the_module in

  let fmat_tocsv_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t; L.pointer_type i8_t |] in 
  let fmat_tocsv = L.declare_function "fmat_tocsv" fmat_tocsv_t the_module in 

  let finverse_t = L.var_arg_function_type (L.pointer_type float_mat_struct) [| L.pointer_type i8_t |] in
  let finverse = L.declare_function "finverse" finverse_t the_module in


  let to_imp str = raise (Failure ("Not yet implemented: " ^ str)) in


  (* Add function names and formas to the Stringmap *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types = Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals) in
      let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m 
    in
    List.fold_left function_decl StringMap.empty functions 
  in


  let build_function fdecl =


    (* int main() {}  ----->  define i32 @main() {}  *)
(*     let main_ty = L.function_type (ltype_ of_typ fdecl.styp) [||] in
    let the_function = L.define_function "main" main_ty the_module in
 *)

    (* Find the name of a function in function_decls stringmap *)
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in

    (* An LLVM "instruction builder" points to a basic block.
     * Adding a new instruction mutates both the_module and builder. *)
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let string_format_str = L.build_global_stringptr "%s\n" "fmt" builder 
    and float_format_str = L.build_global_stringptr "%f\n" "fmt" builder
    and int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in

    let ht = Hashtbl.create 10 in
    let local_vars =
      (* Allocate space for any formally declared variables and initialize the value
       * and add the resulting registers to our map *)
      let add_formal m (t, n) p =
        let () = L.set_value_name n p in
        let local = L.build_alloca (ltype_of_typ t) n builder in
        let _ = L.build_store p local builder in
      (* StringMap.add n local m *)
        let _ =  Hashtbl.add m n local in
        m
      in
      List.fold_left2 add_formal ht fdecl.sformals (Array.to_list (L.params the_function))
    in


    let mat_type p = match L.string_of_lltype (L.type_of p) with
      | "%int_mat_struct*" -> L.pointer_type int_mat_struct
      | "%float_mat_struct*" -> L.pointer_type float_mat_struct
      | "%string_mat_struct*" -> L.pointer_type string_mat_struct
      | "%bool_mat_struct*" -> L.pointer_type bool_mat_struct
      | "%char_mat_struct*" -> L.pointer_type char_mat_struct
      | _ -> raise(Failure("This is not a mat type!"))
    in
    let check_mat p = match L.string_of_lltype (L.type_of p) with
      | "%int_mat_struct*" -> true
      | "%float_mat_struct*" -> true
      | "%string_mat_struct*" -> true
      | "%bool_mat_struct*" -> true
      | "%char_mat_struct*" -> true
      | _ -> false
    in
    let add_local (t, n) p =
      let _ = L.set_value_name n p in
      let local_var =
        match t with
        | _ ->
          let typ = if check_mat p then mat_type p else ltype_of_typ t in
          let result = L.build_alloca typ n builder in
          let _ = L.build_store p result builder in result
      in
      Hashtbl.add local_vars n local_var
    in



    let lookup n = try Hashtbl.find local_vars n with Not_found -> 
        raise(Failure("n: " ^ n))
    in


    let rec expr builder ((_, e) : sexpr) = match e with
      | SLit i -> L.const_int i32_t i
      | SStringLit s -> L.build_global_stringptr s "tmp" builder
      | SAssign (s, e) -> let e' = expr builder e in
                          let _ = L.build_store e' (lookup s) builder in e'
      | SId s -> L.build_load (lookup s) s builder
      | SNoExpr -> L.const_null i32_t
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SFLit l -> L.const_float_of_string float_t l
      | SCLit c -> L.build_global_stringptr c "tmp" builder
      (* | SFPoint(s, e) -> *)
        (* let e' = expr builder e in
        let p_e' = L.build_alloca (L.type_of e') "" builder in
        ignore(L.build_store e' p_e' builder);
        let e' = L.build_bitcast p_e' (L.pointer_type i8_t) "" builder in
         *)
        (* let e' = expr builder ((Void, SCall(s, e))) in
        let p_e' = L.build_alloca (L.type_of e') "" builder in
        ignore(L.build_store e' p_e' builder);
        let e' = L.build_bitcast p_e' (L.pointer_type i8_t) "" builder in
        ignore(L.build_store e' p_e' builder);
        L.build_bitcast p_e' (L.pointer_type i8_t) "" builder 
(*  *)
        let func_ e = expr builder e in
        let list_of_args = List.map func_ e in
        let point = L.build_call len_func (Array.of_list list_of_args) "fpoint" builder in
        let p_e' = L.build_alloca (L.type_of point) "" builder in
        ignore(L.build_store e' p_e' builder);
        L.build_bitcast p_e' (L.pointer_type i8_t) "" builder *)
        

      | SListLit l ->
        let ty = match l with
          | hd :: _ -> let (t, _) = hd in
            (match t with
              | A.Int -> L.pointer_type i32_t
              | A.Float -> L.pointer_type float_t
              | A.String -> L.pointer_type string_t
              | A.Bool -> L.pointer_type i1_t
              | A.Char -> L.pointer_type i1_t
              | _ -> raise(Failure("Not a valid list type")))
          | [] -> raise(Failure("Cannot be an empty list"))
        in let size = List.length l
        in let init = L.build_array_malloc ty (L.const_int i32_t size) "tmp" builder
        in let init_array = L.build_pointercast init ty "tmp" builder
        in let setValues index value =
          let pointer = L.build_gep init_array [| L.const_int i32_t index |] "tmp" builder in
          ignore(L.build_store value pointer builder)
        in let _ = List.iteri setValues (List.map (expr builder) l) in
        let struct_type = (match L.string_of_lltype ty with
          | "i32*" -> int_array_struct
          | "double*" -> float_array_struct
          | "i8*" -> string_array_struct
          | "i1*" -> char_array_struct
          | _ -> raise(Failure("Invalid List type"))) in
        let array_struct = L.build_malloc struct_type "tmp" builder in
        let array_length = L.build_struct_gep array_struct 0 "array_length" builder in
        ignore(L.build_store (L.const_int i32_t size ) array_length builder);
        let array_content = L.build_struct_gep array_struct 1 "array_content" builder in
        ignore(L.build_store init_array array_content builder);
        array_struct
      | SListIndex (v, e) ->
        let struct_array = L.build_load (lookup v) "" builder in
        let struct_array = L.build_struct_gep struct_array 1 "" builder in
        let local_array = L.build_load struct_array "" builder in
        let pointer = L.build_gep local_array [| expr builder e |] "" builder in
        L.build_load pointer "" builder
      | SListIndexAssign(v, e1, e2) ->
        let struct_array = L.build_load (lookup v) "" builder in
        let struct_array = L.build_struct_gep struct_array 1 "" builder in
        let local_array = L.build_load struct_array "" builder in
        let pointer = L.build_gep local_array [| expr builder e1 |] "" builder in
        L.build_store (expr builder e2) pointer builder

      | SMatLit rows ->

        let first_ele lst = match lst with
          | hd :: _ -> hd
          | [] -> raise(Failure("empty matrix"))
        in 
        let outer_ty = match rows with
          | hd :: _ -> let (t, _) = (first_ele hd) in
            (match t with
              | A.Int -> L.pointer_type (L.pointer_type int_array_struct)
              | A.Float -> L.pointer_type (L.pointer_type float_array_struct)
              | A.String -> L.pointer_type (L.pointer_type string_array_struct)
              | A.Bool -> L.pointer_type (L.pointer_type bool_array_struct)
              | A.Char -> L.pointer_type (L.pointer_type char_array_struct)
              | _ -> L.pointer_type (L.pointer_type int_array_struct))
          | [] -> L.pointer_type (L.pointer_type int_array_struct)
        in
        let inner_ty = match rows with 
          | hd :: _ -> let (t, _) = (first_ele hd) in
            (match t with
              | A.Int -> L.pointer_type i32_t
              | A.Float -> L.pointer_type float_t
              | A.String -> L.pointer_type i8_t
              | A.Bool -> L.pointer_type i1_t
              | A.Char -> L.pointer_type i1_t
              | _ -> L.pointer_type i32_t)
          | [] -> L.pointer_type i32_t
        in 
        (* # of int_array_structs *)
        let outer_size = List.length rows 
        (* Mallocs outer_size poitners to int_array_structs *)
        in let init = L.build_array_malloc outer_ty (L.const_int i32_t outer_size) "outer" builder
        in let init_array = L.build_pointercast init outer_ty "outer" builder
        
        in let setValues index value inp_array =
          let pointer = L.build_gep inp_array [| L.const_int i32_t index |] "tmp" builder in
          ignore(L.build_store value pointer builder)


        in let innerLists index innerList =
          let inner_size = List.length innerList in
          let init_inner = L.build_array_malloc inner_ty (L.const_int i32_t inner_size) "inner" builder
          in let init_inner_array = L.build_pointercast init_inner inner_ty "inner" builder
          in let inter index value = setValues index value init_inner_array
          in let _ = List.iteri inter (List.map (expr builder) (innerList))

          in
          let inner_struct_type = (match L.string_of_lltype inner_ty with
            | "i32*" -> int_array_struct
            | "double*" -> float_array_struct
            | "i8*" -> string_array_struct
            | "i1*" -> char_array_struct
            | _ -> raise(Failure("ERROR 1"))) in
          let array_inner_struct = L.build_malloc inner_struct_type "tmp" builder in
          let array_length = L.build_struct_gep array_inner_struct 0 "array_length" builder in
          ignore(L.build_store (L.const_int i32_t inner_size ) array_length builder);
          let array_content = L.build_struct_gep array_inner_struct 1 "array_content" builder in
          ignore(L.build_store init_inner_array array_content builder);
          let pointer = L.build_gep init_array [| L.const_int i32_t index |] "outer" builder
          in ignore(L.build_store array_inner_struct pointer builder);
        in let _ = List.iteri innerLists ( List.rev rows)

        (* Create a struct for the int_mat_struct *)
        in let struct_type = (match L.string_of_lltype outer_ty with
          | "%int_array_struct**" -> int_mat_struct
          | "%float_array_struct**" -> float_mat_struct
          | "%string_array_struct**" -> string_mat_struct
          | "%char_array_struct**" -> char_mat_struct
          | "%bool_array_struct**" -> bool_mat_struct
          | _ -> raise(Failure("Failure" ^ L.string_of_lltype outer_ty))) in
        let array_struct_outer = L.build_malloc struct_type "tmp" builder in
        
        let array_length_outer = L.build_struct_gep array_struct_outer 0 "array_length" builder in
        

        ignore(L.build_store (L.const_int i32_t outer_size ) array_length_outer builder);
        let array_content_outer = L.build_struct_gep array_struct_outer 1 "array_content" builder in
        let _ = L.build_store init_array array_content_outer builder
        in array_struct_outer

        
      | SMatIndex (v, e) ->
        (* Since we're only allowing 2D matricies fix the semant so it's not a list expr to make it way less difficult *)
        let (outer_index, inner_index) = match e with
          | hd :: tl :: [] -> (expr builder hd, expr builder tl)
          | _ -> raise(Failure("This is not a 2D matrix"))
        in
        let outer_struct_array = L.build_load (lookup v) "" builder in
        let outer_struct_array = L.build_struct_gep outer_struct_array 1 "" builder in        
        let outer_local_array = L.build_load outer_struct_array "" builder in
        let outer_pointer = L.build_gep outer_local_array [| inner_index |] "" builder in
        let inner_struct_array = L.build_load outer_pointer "" builder in
        let inner_struct_array = L.build_struct_gep inner_struct_array 1 "" builder in
        let inner_local_array = L.build_load inner_struct_array "" builder in
        let inner_pointer = L.build_gep inner_local_array [| outer_index |] "" builder in
        L.build_load inner_pointer "" builder
      | SMatIndexAssign (v, e1, e2) ->
        let (outer_index, inner_index) = match e1 with
          | hd :: tl :: [] -> (expr builder hd, expr builder tl)
          | _ -> raise(Failure("This is not a 2D matrix"))
        in
        let outer_struct_array = L.build_load (lookup v) "" builder in
        let outer_struct_array = L.build_struct_gep outer_struct_array 1 "" builder in        
        let outer_local_array = L.build_load outer_struct_array "" builder in
        let outer_pointer = L.build_gep outer_local_array [| inner_index |] "" builder in
        let inner_struct_array = L.build_load outer_pointer "" builder in
        let inner_struct_array = L.build_struct_gep inner_struct_array 1 "" builder in
        let inner_local_array = L.build_load inner_struct_array "" builder in
        let inner_pointer = L.build_gep inner_local_array [| outer_index |] "" builder in
        L.build_store (expr builder e2) inner_pointer builder
      | SBinop (e1, op, e2) ->
          let e1' = expr builder e1
          and e2' = expr builder e2 in
          (match (L.string_of_lltype (L.type_of e2')) with 
            | "%int_array_struct*" ->
              (match (L.string_of_lltype (L.type_of e1')) with
              | "%int_array_struct*" ->
              (match op with
                | A.Add ->  
                  L.build_call add_list_func [| expr builder e1; expr builder e2 |] "add_list" builder
                | A.Sub ->
                  L.build_call sub_list_func [| expr builder e1; expr builder e2 |] "sub_list" builder
                | A.Dot ->
                  L.build_call dot_prod_func [| expr builder e1; expr builder e2 |] "dot_prod_int" builder
                | A.Mult_M ->
                  L.build_call elem_mult_list_func [| expr builder e1; expr builder e2|] "elem_mult_list_int" builder
                | A.Div_M ->
                  L.build_call elem_div_list_func [| expr builder e1; expr builder e2|] "elem_div_list_int" builder
                | _ -> raise(Failure("Either invalid operator or not implemented yet"))
              )
              | "i32" ->
               ( match op with
                | A.Add ->
                  L.build_call const_add_list_func [| expr builder e1; expr builder e2|] "const_add_list_int" builder
                | A.Mult ->
                  L.build_call const_mult_list_func [| expr builder e1; expr builder e2|] "const_mult_list_int" builder
                | _ -> raise(Failure("Either invalid operator or not implemented yet"))
            )
                | _ -> raise(Failure("Either invalid operator or not implemented yet"))
            )
            | "%float_array_struct*" ->
              (match (L.string_of_lltype (L.type_of e1')) with
              | "%float_array_struct*" ->
              (match op with
                | A.Add ->  
                  L.build_call add_list_func_float [| expr builder e1; expr builder e2 |] "add_list" builder
                | A.Sub ->
                  L.build_call sub_list_func_float [| expr builder e1; expr builder e2 |] "sub_list" builder
                | A.Dot ->
                  L.build_call dot_prod_func_float [| expr builder e1; expr builder e2 |] "dot_prod_float" builder
                | A.Mult_M ->
                  L.build_call elem_mult_list_func_float [| expr builder e1; expr builder e2|] "elem_mult_list_float" builder
                | A.Div_M ->
                  L.build_call elem_div_list_func_float [| expr builder e1; expr builder e2|] "elem_div_list_float" builder
                | _ -> raise(Failure("Either invalid operator or not implemented yet"))
              )
              | "double" ->
              ( match op with
                | A.Add ->
                  L.build_call const_add_list_func_float [| expr builder e1; expr builder e2|] "const_add_list_float" builder
                | A.Mult ->
                  L.build_call const_mult_list_func_float [| expr builder e1; expr builder e2|] "const_mult_list_float" builder
                | _ -> raise(Failure("Either invalid operator or not implemented yet"))
              )
              | _ -> raise(Failure("Either invalid operator or not implemented yet"))
              )  
            | "%int_mat_struct*" -> 
              (match (L.string_of_lltype (L.type_of e1')) with
              | "%int_mat_struct*" ->
              (match op with
                | A.Add ->  
                  let p_e1' = L.build_alloca (L.type_of e1') "" builder in
                  ignore(L.build_store e1' p_e1' builder);
                  let e1' = L.build_bitcast p_e1' (L.pointer_type i8_t) "" builder in
                  let p_e2' = L.build_alloca (L.type_of e2') "" builder in
                  ignore(L.build_store e2' p_e2' builder);
                  let e2' = L.build_bitcast p_e2' (L.pointer_type i8_t) "" builder in
                  L.build_call add_mat_func [| e1'; e2' |] "add_mat" builder
                | A.Sub ->
                  let p_e1' = L.build_alloca (L.type_of e1') "" builder in
                  ignore(L.build_store e1' p_e1' builder);
                  let e1' = L.build_bitcast p_e1' (L.pointer_type i8_t) "" builder in

                  let p_e2' = L.build_alloca (L.type_of e2') "" builder in
                  ignore(L.build_store e2' p_e2' builder);
                  let e2' = L.build_bitcast p_e2' (L.pointer_type i8_t) "" builder in
                  L.build_call sub_mat_func [| e1'; e2' |] "sub_mat" builder
                | A.Mult ->
                  let p_e1' = L.build_alloca (L.type_of e1') "" builder in
                  ignore(L.build_store e1' p_e1' builder);
                  let e1' = L.build_bitcast p_e1' (L.pointer_type i8_t) "" builder in
                  let p_e2' = L.build_alloca (L.type_of e2') "" builder in
                  ignore(L.build_store e2' p_e2' builder);
                  let e2' = L.build_bitcast p_e2' (L.pointer_type i8_t) "" builder in
                  L.build_call mult_mat_func [| e1'; e2' |] "mult_mat" builder 
                | A.Mult_M ->
                  let p_e1' = L.build_alloca (L.type_of e1') "" builder in
                  ignore(L.build_store e1' p_e1' builder);
                  let e1' = L.build_bitcast p_e1' (L.pointer_type i8_t) "" builder in
                  let p_e2' = L.build_alloca (L.type_of e2') "" builder in
                  ignore(L.build_store e2' p_e2' builder);
                  let e2' = L.build_bitcast p_e2' (L.pointer_type i8_t) "" builder in
                  L.build_call elem_mult_mat_func [| e1'; e2' |] "elem_mult_mat_int" builder 
                | A.Div_M ->
                  let p_e1' = L.build_alloca (L.type_of e1') "" builder in
                  ignore(L.build_store e1' p_e1' builder);
                  let e1' = L.build_bitcast p_e1' (L.pointer_type i8_t) "" builder in
                  let p_e2' = L.build_alloca (L.type_of e2') "" builder in
                  ignore(L.build_store e2' p_e2' builder);
                  let e2' = L.build_bitcast p_e2' (L.pointer_type i8_t) "" builder in
                  L.build_call elem_div_mat_func [| e1'; e2' |] "elem_div_mat_int" builder 
                
                | _ -> raise(Failure("Either invalid operator or not implemented yet"))
              ) 
              | "i32" ->
               ( match op with
                | A.Add ->
                  let p_e2' = L.build_alloca (L.type_of e2') "" builder in
                  ignore(L.build_store e2' p_e2' builder);
                  let e2' = L.build_bitcast p_e2' (L.pointer_type i8_t) "" builder in
                  L.build_call const_add_mat_func [| expr builder e1; e2'|] "const_add_mat_int" builder
                | A.Mult ->
                  let p_e2' = L.build_alloca (L.type_of e2') "" builder in
                  ignore(L.build_store e2' p_e2' builder);
                  let e2' = L.build_bitcast p_e2' (L.pointer_type i8_t) "" builder in
                  L.build_call const_mult_mat_func [| expr builder e1; e2'|] "const_mult_mat_int" builder
                | _ -> raise(Failure("Either invalid operator or not implemented yet"))
            )
                | _ -> raise(Failure("Either invalid operator or not implemented yet"))
            )
            | "%float_mat_struct*" -> 
              (match (L.string_of_lltype (L.type_of e1')) with
              | "%float_mat_struct*" ->
              (match op with
                | A.Add ->  
                  let p_e1' = L.build_alloca (L.type_of e1') "" builder in
                  ignore(L.build_store e1' p_e1' builder);
                  let e1' = L.build_bitcast p_e1' (L.pointer_type i8_t) "" builder in

                  let p_e2' = L.build_alloca (L.type_of e2') "" builder in
                  ignore(L.build_store e2' p_e2' builder);
                  let e2' = L.build_bitcast p_e2' (L.pointer_type i8_t) "" builder in
                  L.build_call add_mat_func_float [| e1'; e2' |] "add_mat" builder
                | A.Sub ->
                  let p_e1' = L.build_alloca (L.type_of e1') "" builder in
                  ignore(L.build_store e1' p_e1' builder);
                  let e1' = L.build_bitcast p_e1' (L.pointer_type i8_t) "" builder in

                  let p_e2' = L.build_alloca (L.type_of e2') "" builder in
                  ignore(L.build_store e2' p_e2' builder);
                  let e2' = L.build_bitcast p_e2' (L.pointer_type i8_t) "" builder in
                  L.build_call sub_mat_func_float [| e1'; e2' |] "sub_mat" builder
                | A.Mult ->
                  let p_e1' = L.build_alloca (L.type_of e1') "" builder in
                  ignore(L.build_store e1' p_e1' builder);
                  let e1' = L.build_bitcast p_e1' (L.pointer_type i8_t) "" builder in

                  let p_e2' = L.build_alloca (L.type_of e2') "" builder in
                  ignore(L.build_store e2' p_e2' builder);
                  let e2' = L.build_bitcast p_e2' (L.pointer_type i8_t) "" builder in


                  L.build_call mult_mat_func_float [| e1'; e2' |] "mult_mat" builder
                | A.Mult_M ->
                  let p_e1' = L.build_alloca (L.type_of e1') "" builder in
                  ignore(L.build_store e1' p_e1' builder);
                  let e1' = L.build_bitcast p_e1' (L.pointer_type i8_t) "" builder in
                  let p_e2' = L.build_alloca (L.type_of e2') "" builder in
                  ignore(L.build_store e2' p_e2' builder);
                  let e2' = L.build_bitcast p_e2' (L.pointer_type i8_t) "" builder in
                  L.build_call elem_mult_mat_func_float [| e1'; e2' |] "elem_mult_mat_float" builder 
                | A.Div_M ->
                  let p_e1' = L.build_alloca (L.type_of e1') "" builder in
                  ignore(L.build_store e1' p_e1' builder);
                  let e1' = L.build_bitcast p_e1' (L.pointer_type i8_t) "" builder in
                  let p_e2' = L.build_alloca (L.type_of e2') "" builder in
                  ignore(L.build_store e2' p_e2' builder);
                  let e2' = L.build_bitcast p_e2' (L.pointer_type i8_t) "" builder in
                  L.build_call elem_div_mat_func_float [| e1'; e2' |] "elem_div_mat_float" builder 
                | _ -> raise(Failure("Either invalid operator or not implemented yet"))
              )
              | "double" ->
               ( match op with
                | A.Add ->
                  let p_e2' = L.build_alloca (L.type_of e2') "" builder in
                  ignore(L.build_store e2' p_e2' builder);
                  let e2' = L.build_bitcast p_e2' (L.pointer_type i8_t) "" builder in
                  L.build_call const_add_mat_func_float [| expr builder e1; e2'|] "const_add_mat_float" builder
                | A.Mult ->
                  let p_e2' = L.build_alloca (L.type_of e2') "" builder in
                  ignore(L.build_store e2' p_e2' builder);
                  let e2' = L.build_bitcast p_e2' (L.pointer_type i8_t) "" builder in
                  L.build_call const_mult_mat_func_float [| expr builder e1; e2'|] "const_mult_mat_float" builder
                | _ -> raise(Failure("Either invalid operator or not implemented yet"))
              )
                | _ -> raise(Failure("Either invalid operator or not implemented yet"))
            )
            | "double" ->
              (match op with
                | A.Add     -> L.build_fadd
                | A.Sub     -> L.build_fsub
                | A.Mult    -> L.build_fmul
                | A.Div     -> L.build_fdiv
                | A.Eq      -> L.build_fcmp L.Fcmp.Oeq
                | A.Neq     -> L.build_fcmp L.Fcmp.One
                | A.Less    -> L.build_fcmp L.Fcmp.Olt
                | A.Leq     -> L.build_fcmp L.Fcmp.Ole
                | A.Greater -> L.build_fcmp L.Fcmp.Ogt
                | A.Geq     -> L.build_fcmp L.Fcmp.Oge
                | _         -> raise(Failure("Unsupported operations"))
              ) e1' e2' "tmp" builder
            | _ -> 
              (match op with
              | A.Add     -> L.build_add
              | A.Sub     -> L.build_sub
              | A.Mult    -> L.build_mul
              | A.Div     -> L.build_sdiv
              | A.Mod     -> L.build_srem
              | A.And     -> L.build_and
              | A.Or      -> L.build_or
              | A.Dot     -> raise(Failure("Not valid for this type"))
              | A.Mult_M     -> raise(Failure("Not valid for this type"))
              | A.Div_M     -> raise(Failure("Not valid for this type"))
              | A.Eq      -> L.build_icmp L.Icmp.Eq
              | A.Neq     -> L.build_icmp L.Icmp.Ne
              | A.Less    -> L.build_icmp L.Icmp.Slt
              | A.Leq     -> L.build_icmp L.Icmp.Sle
              | A.Greater -> L.build_icmp L.Icmp.Sgt
              | A.Geq     -> L.build_icmp L.Icmp.Sge
              ) e1' e2' "tmp" builder)
      | SUnop(op, e) ->
	        let (t, _) = e in
          let e' = expr builder e in
          (match (L.string_of_lltype (L.type_of e')) with
            | "%int_mat_struct*" ->
              (match op with
                | A.Trans_M -> 
                    let p_e' = L.build_alloca (L.type_of e') "" builder in
                    ignore(L.build_store e' p_e' builder);
                    let e' = L.build_bitcast p_e' (L.pointer_type i8_t) "" builder in
                    L.build_call trans_mat_func [| e' |] "trans_mat" builder
                | _ -> raise(Failure("Either invalid operator or not implemented yet: " ^ (A.string_of_uop op)))
              )
            | "%float_mat_struct*" ->
              (match op with
                | A.Trans_M -> 
                    let p_e' = L.build_alloca (L.type_of e') "" builder in
                    ignore(L.build_store e' p_e' builder);
                    let e' = L.build_bitcast p_e' (L.pointer_type i8_t) "" builder in
                    L.build_call trans_mat_func_float [| e' |] "trans_mat" builder
                | _ -> raise(Failure("Either invalid operator or not implemented yet: " ^ (A.string_of_uop op)))
              )
            | _ ->
    	        (match op with
    	           A.Neg when t = A.Float -> L.build_fneg
    	          | A.Neg                  -> L.build_neg
                | A.Not                  -> L.build_not
                | _ -> raise(Failure("Either invalid operator or not implemented yet: " ^ (A.string_of_uop op)))) e' "tmp" builder)
      | SPunop(v, op) ->
        let e' = L.build_load (lookup v) v builder in
        let inc = L.build_add (L.const_int i32_t 1) e' "tmp" builder in
        let dec = L.build_add (L.const_int i32_t (-1)) e' "tmp" builder in
        (match op with
           A.Increment            -> L.build_store inc
         | A.Decrement            -> L.build_store dec) (lookup v) builder
      | SCall("printstr", [e]) ->
        L.build_call printf_func [| string_format_str; (expr builder e) |] "printf" builder
      | SCall("len_mat", [e]) ->
        let e' = expr builder e in
        let p_e' = L.build_alloca (L.type_of e') "" builder in
        ignore(L.build_store e' p_e' builder);
        let e' = L.build_bitcast p_e' (L.pointer_type i8_t) "" builder in 
        L.build_call len_func_mat [| e' |] "len" builder 
      | SCall("len_mat_float", [e]) -> 
        let e' = expr builder e in
        let p_e' = L.build_alloca (L.type_of e') "" builder in
        ignore(L.build_store e' p_e' builder);
        let e' = L.build_bitcast p_e' (L.pointer_type i8_t) "" builder in 
        L.build_call len_func_mat_float [| e' |] "len_mat_float" builder 
      | SCall("read_intmat_from_file", [file_name]) ->
        let e' = expr builder file_name in
        L.build_call read_intmat_from_file_func [| e' |] "" builder
      | SCall("write_string_to_file", [file_name;content]) ->
        let e1 = expr builder file_name in
        let e2 = expr builder content in
        L.build_call write_string_to_file_func [| e1 ; e2 |] "" builder
      | SCall("len", [e]) ->
        let e' = expr builder e in
        let p_e' = L.build_alloca (L.type_of e') "" builder in
        ignore(L.build_store e' p_e' builder);
        let e' = L.build_bitcast p_e' (L.pointer_type i8_t) "" builder in             
        L.build_call len_func [| e'(* expr builder e *) |] "len" builder
      | SCall("append", [e;el]) -> 
        let e' = expr builder e in 
        (* let _ = Printf.printf "%s"  (L.string_of_lltype (L.type_of e'))in  *)
        let p_e' = L.build_alloca (L.type_of e') "" builder in 
          ignore(L.build_store e' p_e' builder);
        let e' = L.build_bitcast p_e' (L.pointer_type i8_t) "" builder in
        let el' = expr builder el in 
        let ptr_el' = L.build_alloca (L.type_of el') "" builder in 
          ignore(L.build_store el' ptr_el' builder);
        let el' = L.build_bitcast ptr_el' (L.pointer_type i8_t) "" builder in
        L.build_call append_func [| e' ; el' |] "" builder
      | SCall("fmat_tocsv", [e;path]) -> 
        let e' = expr builder e in 
        let p_e' = L.build_alloca (L.type_of e') "" builder in 
          ignore(L.build_store e' p_e' builder);
        let e' = L.build_bitcast p_e' (L.pointer_type i8_t) "" builder in 
        let path' = expr builder path in 
        let ptr_path' = L.build_alloca (L.type_of path') "" builder in 
          ignore(L.build_store path' ptr_path' builder);
        let path' = L.build_bitcast ptr_path' (L.pointer_type i8_t) "" builder in 
        L.build_call fmat_tocsv [| e';path' |] "" builder 
      | SCall("det_int", [e]) ->
        let e' = expr builder e in
        let p_e' = L.build_alloca (L.type_of e') "" builder in
        ignore(L.build_store e' p_e' builder);
        let e' = L.build_bitcast p_e' (L.pointer_type i8_t) "" builder in 
        L.build_call det_mat_func [| e' |] "" builder 
      | SCall("det_float", [e]) ->
        let e' = expr builder e in
        let p_e' = L.build_alloca (L.type_of e') "" builder in
        ignore(L.build_store e' p_e' builder);
        let e' = L.build_bitcast p_e' (L.pointer_type i8_t) "" builder in 
        L.build_call det_mat_func_float [| e' |] "" builder 
      | SCall("is_square", [e]) ->
        let e' = expr builder e in
        let p_e' = L.build_alloca (L.type_of e') "" builder in
        ignore(L.build_store e' p_e' builder);
        let e' = L.build_bitcast p_e' (L.pointer_type int_mat_struct) "" builder in 
        L.build_call is_square_func [| e' |] "" builder 
      | SCall ("printint", [e]) ->
        L.build_call printf_intfunc [| int_format_str ; (expr builder e) |] "printf" builder
      | SCall ("print_intlist", [e]) ->
        let p_e' = L.build_alloca (L.type_of (expr builder e)) "" builder in
        ignore(L.build_store (expr builder e) p_e' builder);
        let e' = L.build_bitcast p_e' (L.pointer_type i8_t) "" builder in
        L.build_call printf_list_int_func [| e' |] "printlist" builder
      | SCall ("print_floatlist", [e]) ->
        let p_e' = L.build_alloca (L.type_of (expr builder e)) "" builder in
        ignore(L.build_store (expr builder e) p_e' builder);
        let e' = L.build_bitcast p_e' (L.pointer_type i8_t) "" builder in
        L.build_call printf_list_float_func [| e' |] "printlist" builder
      | SCall ("print_intmat", [e; pretty]) ->
        let p_e' = L.build_alloca (L.type_of (expr builder e)) "" builder in
        ignore(L.build_store (expr builder e) p_e' builder);
        let e' = L.build_bitcast p_e' (L.pointer_type i8_t) "" builder in
        L.build_call printf_mat_int_func [| e' ; (expr builder pretty) |] "printmat" builder 
      | SCall ("print_floatmat", [e; pretty]) ->
        let p_e' = L.build_alloca (L.type_of (expr builder e)) "" builder in
        ignore(L.build_store (expr builder e) p_e' builder);
        let e' = L.build_bitcast p_e' (L.pointer_type i8_t) "" builder in
        L.build_call printf_mat_float_func [| e' ; (expr builder pretty) |] "printmat" builder 
      | SCall("fmat_fromcsv", [e]) -> 
        let p_e' = L.build_alloca (L.type_of (expr builder e)) "" builder in 
          ignore(L.build_store (expr builder e) p_e' builder);
        let e' = L.build_bitcast p_e' (L.pointer_type i8_t) "" builder in 
        L.build_call fmat_csv [| e' |] "" builder
      | SCall("finverse", [e]) -> 
        let p_e' = L.build_alloca (L.type_of (expr builder e)) "" builder in 
          ignore(L.build_store (expr builder e) p_e' builder);
        let e' = L.build_bitcast p_e' (L.pointer_type i8_t) "" builder in 
        L.build_call finverse [| e' |] "" builder
      | SCall ("printfloat", [e]) ->
        L.build_call printf_float_func [| float_format_str ; (expr builder e) |] "printf" builder
      | SCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let llargs = List.rev (List.map (expr builder) (List.rev args)) in
        let result = (match fdecl.styp with
          A.Void -> ""
          | _ -> f ^ "_result") in
        L.build_call fdef (Array.of_list llargs) result builder
      | _ -> raise(Failure("Unimplemented"))
    in


    let add_terminal builder instr =
      (* The current block where we're inserting instr *)
      match L.block_terminator (L.insertion_block builder) with
      Some _ -> ()
    | None -> ignore (instr builder) in

    let rec stmt builder = function
      | SExpr e -> let _ = expr builder e in builder
      | SVdecl (typ, id, e) -> let _ = add_local (typ, id) (expr builder (typ, e)) in builder
      | SBlock sl -> List.fold_left stmt builder sl
      (* return 0;  ----->  ret i32 0 *)
      | SReturn e -> let _ = match fdecl.styp with
                              A.Int -> L.build_ret (expr builder e) builder
                            | A.Bool -> L.build_ret (expr builder e) builder
                            | A.Char -> L.build_ret (expr builder e) builder
                            | A.String -> L.build_ret (expr builder e) builder
                            | A.Float -> L.build_ret (expr builder e) builder
                            | _ -> to_imp (A.string_of_typ fdecl.styp)
                     in builder
      | SIf(predicate, then_stmt, else_stmt) ->
        let bool_val = expr builder predicate in
        let merge_bb = L.append_block context "merge" the_function in
        let branch_instr = L.build_br merge_bb in

        let then_bb = L.append_block context "then" the_function in
        let then_builder = stmt (L.builder_at_end context then_bb) then_stmt in
        let () = add_terminal then_builder branch_instr in

        let else_bb = L.append_block context "else" the_function in
        let else_builder = stmt (L.builder_at_end context else_bb) else_stmt in
        let () = add_terminal else_builder branch_instr in

        let _ = L.build_cond_br bool_val then_bb else_bb builder in
          L.builder_at_end context merge_bb

      | SWhile(predicate, body) ->
        let pred_bb = L.append_block context "while" the_function in
      (* In current block, branch to predicate to execute the condition *)
        let _ = L.build_br pred_bb builder in

              (* Create the body's block, generate the code for it, and add a branch
              back to the predicate block (we always jump back at the end of a while
              loop's body, unless we returned or something) *)
        let body_bb = L.append_block context "while_body" the_function in
        ignore(add_terminal (stmt (L.builder_at_end context body_bb) body) (L.build_br pred_bb));

              (* Generate the predicate code in the predicate block *)
        let pred_builder = L.builder_at_end context pred_bb in
        let bool_val = expr pred_builder (* pred_builder *) predicate in

              (* Hook everything up *)
        let merge_bb = L.append_block context "merge" the_function in
        ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
        L.builder_at_end context merge_bb 
      | SFor (e1, e2, e3, body) -> stmt builder
        ( SBlock [e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
      (*| s -> to_imp (string_of_sstmt s)*)
    in ignore (stmt builder (SBlock fdecl.sbody))

  in List.iter build_function functions; the_module
