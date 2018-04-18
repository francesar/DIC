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

  and the_module = L.create_module context "DIC" in

  let ltype_of_typ = function
    | A.Int -> i32_t
    | A.String -> string_t
    | A.Void  -> void_t
    | A.Bool -> i1_t
    | A.Float -> float_t
    | A.Char -> i1_t
    | t -> raise (Failure ("Type " ^ A.string_of_typ t ^ " not implemented yet"))
  in

  let printf_t : L.lltype =
    L.var_arg_function_type string_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in


  let printf_int = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_intfunc = L.declare_function "printf" printf_int the_module in


  let to_imp str = raise (Failure ("Not yet implemented: " ^ str)) in


  (* Add function names and formas to the Stringmap *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
  Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in


  let build_function fdecl =


    (* int main() {}  ----->  define i32 @main() {}  *)
(*     let main_ty = L.function_type (ltype_of_typ fdecl.styp) [||] in
    let the_function = L.define_function "main" main_ty the_module in
 *)

    (* Find the name of a function in function_decls stringmap *)
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in

    (* An LLVM "instruction builder" points to a basic block.
     * Adding a new instruction mutates both the_module and builder. *)
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let string_format_str = L.build_global_stringptr "%s\n" "fmt" builder
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

    let add_local (t, n) p =
      (* let _ = match p with
        | SNoExpr -> p
        | sx -> L.set_value_name n p in *)
      let _ = L.set_value_name n p in
      let _ = Printf.printf "%s\n" n in
      let local_var = L.build_alloca (ltype_of_typ t) n builder in
      (* let _ = match p with
        | NoExpr p -> p
        | p -> L.build_store p local builder in  *)
      let _ = L.build_store p local_var builder in
      (* StringMap.add n local_var local_vars *)
      Hashtbl.add local_vars n local_var
    in



    let lookup n = try Hashtbl.find local_vars n
                      with Not_found -> raise(Failure("n: " ^ n))
                   (* with Not_found -> StringMap.find n global_vars *)
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
      | SBinop (e1, op, e2) ->
(*           let (t, _) = e1 *)
          let e1' = expr builder e1
          and e2' = expr builder e2 in
          (match op with
          | A.Add     -> L.build_add
          | A.Sub     -> L.build_sub
          | A.Mult    -> L.build_mul
          | A.Div     -> L.build_sdiv
          (* | A.Mod     -> L.build_mod *)
          | A.And     -> L.build_and
          | A.Or      -> L.build_or
          | A.Eq      -> L.build_icmp L.Icmp.Eq
          | A.Neq     -> L.build_icmp L.Icmp.Ne
          | A.Less    -> L.build_icmp L.Icmp.Slt
          | A.Leq     -> L.build_icmp L.Icmp.Sle
          | A.Greater -> L.build_icmp L.Icmp.Sgt
          | A.Geq     -> L.build_icmp L.Icmp.Sge
          ) e1' e2' "tmp" builder
      | SUnop(op, e) ->
	        let (t, _) = e in
          let e' = expr builder e in
	        (match op with
	           A.Neg when t = A.Float -> L.build_fneg
	          | A.Neg                  -> L.build_neg
            | A.Not                  -> L.build_not) e' "tmp" builder
      | SPunop(e, op) ->
        let e' = expr builder e in
        (match op with
           A.Increment            -> L.build_add (L.const_int i32_t 1)
         | A.Decrement            -> L.build_add (L.const_int i32_t (-1)))e' "tmp" builder
      | SCall("printstr", [e]) ->
        L.build_call printf_func [| string_format_str; (expr builder e) |] "printf" builder
      | SCall ("printint", [e]) ->
        L.build_call printf_intfunc [| int_format_str ; (expr builder e) |] "printf" builder
      | SCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let llargs = List.rev (List.map (expr builder) (List.rev args)) in
        let result = (match fdecl.styp with
          A.Void -> ""
          | _ -> f ^ "_result") in
        L.build_call fdef (Array.of_list llargs) result builder
      (* | _ -> to_imp (string_of_sexpr (A.Int, e)) *)
      | _ -> to_imp (string_of_sexpr (A.Int, e))
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
              let while_builder = stmt (L.builder_at_end context body_bb) body in
        let () = add_terminal while_builder (L.build_br pred_bb) in

              (* Generate the predicate code in the predicate block *)
        let pred_builder = L.builder_at_end context pred_bb in
        let bool_val = expr pred_builder predicate in

              (* Hook everything up *)
        let merge_bb = L.append_block context "merge" the_function in
        let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in
        L.builder_at_end context merge_bb
      | s -> to_imp (string_of_sstmt s)
    in ignore (stmt builder (SBlock fdecl.sbody))

  in List.iter build_function functions; the_module
