open Ast
open Sast
module StringMap = Map.Make(String)

type func_symbol = func_decl StringMap.t

(* args here might need to change since we accept classes OR statment blocks as valid programs *)
let check (pname, (var_decls, func_decls)) =

    let check_binds (kind : string) (to_check : bind list) =
      let check_it checked binding =
        let void_err = "illegal void " ^ kind ^ " " ^ snd binding
        and dup_err = "duplicate " ^ kind ^ " " ^ snd binding
        in match binding with
          (* No void bindings *)
        | (Void, _) -> raise (Failure void_err)
        | (_, n1) -> match checked with
                      (* No duplicate bindings *)
                      | ((_, n2) :: _) when n1 = n2 -> raise (Failure dup_err)
                      | _ -> binding :: checked
      in let _ = List.fold_left check_it [] (List.sort compare to_check)
         in to_check
    in

    let convert_var_decl (kind : string) (input_list : var_decl list) =
      let to_bind (ty, s, _) = (ty, s) in
      let new_list = List.map to_bind input_list in
      check_binds kind new_list
    in

    let var_decls' = convert_var_decl "var_decls" var_decls in

    let check_binds_in_stmts (kind: string) (to_check : stmt list) =
      (* check whether each stmt is  *)
      let rec check_type (inp : stmt list) = match inp with
        | Vdecl a :: tl -> a :: check_type tl
        | _ :: tl -> check_type tl
        | [] -> []
      in
      let new_list = check_type to_check in
      convert_var_decl kind new_list

    in

  (* FUNCTIONS *)
  let built_in_decls =
    let rec test (inp : typ list) =

      match inp with
      | [] -> [](* raise (Failure ("zero arg " ^ String.concat ", " (List.map string_of_typ inp))) *)
      | hd :: tl -> (hd, "x") :: test tl (* raise (Failure ("zero arg " ^ String.concat ", " (List.map string_of_typ inp))) *)
    in

    StringMap.add "printint" 
      {typ = Void; fname = "printint"; formals = test [Int]; body = []}
      (StringMap.add "printstr"
        {typ = Void; fname = "printstr"; formals = test [Int]; body = []}
      (StringMap.add "add"
        {typ = IntM; fname = "add_list"; formals = test [IntM]; body = []}
      (StringMap.singleton "len"
        {typ = Int; fname = "len"; formals = test [IntM]; body = []})))

(* 
    let add_bind map (ty, name) =

      StringMap.add name {
      typ = ;
      fname = name;
      (* formals = [(ty, "x")]; *)
      formals = test ty;
      body = []
    } map *)
    (* Add built in function declarations into arr here
      Convert any datatype into string for print
    *)
    (* in List.fold_left add_bind StringMap.empty [([Int], "printint");([String], "printstr"); ([IntM], "printlist"); ([], "len")] *)

  in

  (* adding functions to symbol table *)
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err err = raise (Failure err)
    and n = fd.fname
    in

    (* Printing the function name and corresponding args *)
    (* let _ = Printf.printf "\n%s" ("fname: " ^ fd.fname ^ " args: "  ^(String.concat ", " (List.map string_of_binding fd.formals)) ^ "\n" ) in *)

    match fd with
      | _ when StringMap.mem n built_in_decls -> make_err built_in_err
      | _ when StringMap.mem n map -> make_err dup_err
      | _ -> StringMap.add n fd map
  in

  (* Add functions to function symbol table *)
  let function_decls = List.fold_left add_func built_in_decls func_decls
  in

  (* Finds and returns functions in function symbol table *)
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  (* Make sure main function is defined *)
  let _ = find_func "main"
  in


  let check_function func =
    let formals' = check_binds "formal" func.formals in
    let locals' = check_binds_in_stmts "local" func.body in

  (* Raise an exception if the given rvalue type cannot be assigned to
      the given lvalue type *)
    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet
       else if rvaluet == Void then lvaluet
       else raise (Failure err)
    in

    (* Build local symbol table of variables for this function *)
    let symbols = Hashtbl.create 10 in
    let f (ty, name) = Hashtbl.add symbols name ty in
    (*let p (_, name) = Printf.printf "%s\n" name in*)
    let _ = List.iter f (locals') in
    let _ = List.iter f (formals') in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try Hashtbl.find symbols s
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return a semantically-checked expression*)
    let rec expr = function
        Literal l   -> (Int, SLit l)
      | Cliteral l  -> (Char, SCLit l)
      | Fliteral l  -> (Float, SFLit l)
      | BoolLit l   -> (Bool, SBoolLit l)
      | StringLit s -> (String, SStringLit s)
      | Noexpr      -> (Void, SNoExpr)
      | Id s        -> (type_of_identifier s, SId s)
      | Assign(var, e) as ex ->
        let lt = type_of_identifier var
        and (rt, e') = expr e in
        let err = "illegal assignment " ^ var ^ string_of_typ lt ^ " = " ^
                  string_of_typ rt ^ " in " ^ string_of_expr ex
        in (check_assign lt rt err, SAssign(var, (rt, e')))
      | ListLit l   ->
        (* Potentially do this in codegen instead of semant  *)
        (* Take in a list and return first element type *)
        let first_ele inp = match inp with
          | hd :: _ ->(
            let (t, _) = expr hd in
            match t with
              | _ -> t)
          | [] -> Int
        in

        (* Check the folding *)
        let fold_checking init_type ele =
          let (t, _) = expr ele in
          if (init_type = t) then init_type
          else raise (Failure("Type " ^ string_of_typ t ^ " does not match first type " ^ string_of_typ init_type))
        in

        (* Take in a list, get the first element and fold through the rest of the list *)
        let check_ele_consistency inp =
          let first_type = first_ele inp in
          let typ = List.fold_left fold_checking first_type inp
          in typ
        in

        (* Get the type of list *)
        let typ = check_ele_consistency l in
        (* let _ = Printf.printf "%s" (string_of_typ typ) in *)
        let ty = match typ with
          | Int -> IntM
          | Float -> FloatM
          | Char -> CharM
          | String -> StringM
          | Bool -> BoolM
          | _ -> raise(Failure(string_of_typ typ ^ " is not an acceptable list type."))
        in
        (ty, SListLit(List.map expr l))
      | ListIndex(v, e1) ->
        let (t, _) = expr e1 in
        let _ = match t with
          | Int -> Int
          | _ -> raise(Failure("Index must be an Int"))
        in
        let ty = match type_of_identifier v with
          | IntM -> Int
          | FloatM -> Float
          | CharM  -> Char
          | StringM -> String
          | BoolM -> Bool
          | _ -> raise(Failure("error should have been caught before this"))
        in
        (ty, SListIndex(v, expr e1))
      | ListIndexAssign (v, e1, e2) ->

        let (t2, _) = expr e1 in
        let _ = match t2 with
          | Int -> Int
          | _ -> raise(Failure("Index must be an Int"))
        in
        (type_of_identifier v, SListIndexAssign(v, expr e1, expr e2))
      | MatLit(rows) ->
        (* Take in a list and return first element type *)
        let first_ele inp = match inp with
          | hd :: _ ->(
            let (t, _) = expr hd in
            match t with
              | _ -> t)
          | [] -> Int
        in

        (* Check the folding *)
        let fold_checking init_type ele =
          let (t, _) = expr ele in
          if (init_type = t) then init_type
          else raise (Failure("Type " ^ string_of_typ t ^ " does not match first type " ^ string_of_typ init_type))
        in

        (* Take in a list, get the first element and fold through the rest of the list *)
        let check_ele_consistency inp =
          let first_type = first_ele inp in
          let typ = List.fold_left fold_checking first_type inp
          in typ
        in

        let fold_checking_list init_type ele =
          let temp = check_ele_consistency ele in
          if (init_type = temp) then init_type
          else raise (Failure("Type " ^ string_of_typ temp ^ " does not match first type " ^ string_of_typ init_type))
        in

        (* Check matching type of all lists *)
        let check_list_consistency inp =
          let first_list inp2 = match inp2 with
            | hd :: _ -> check_ele_consistency hd
            | [] -> Int
          in
          let first_type = first_list inp in
          let typ = List.fold_left fold_checking_list first_type inp in
          typ
        in

        (* Get the type of list *)
        let typ = check_list_consistency rows in
        let ty = match typ with
          | Int -> IntM
          | Float -> FloatM
          | Char -> CharM
          | String -> StringM
          | Bool -> BoolM
          | _ -> raise(Failure(string_of_typ typ ^ " is not an acceptable list type."))
        in
        (ty, SMatLit(List.map (fun inp -> List.map expr inp) rows))
      | MatIndexAssign (v, e1, e2) ->
        let testIndex expression =
          let (t, _) = expr expression in
          match t with
            | Int -> Int
            | _ -> raise(Failure("Index must be an Int"))
        in
        let _ = List.map testIndex e1 in
        (type_of_identifier v, SMatIndexAssign(v, List.map expr e1, expr e2))
      | MatIndex (v, e1) ->
        let testIndex expression =
          let (t, _) = expr expression in
          match t with
            | Int -> Int
            | _ -> raise(Failure("Index must be an Int"))
        in
        let _ = List.map testIndex e1 in
        let ty = match type_of_identifier v with
          | IntM -> Int
          | FloatM -> Float
          | CharM  -> Char
          | StringM -> String
          | BoolM -> Bool
          | _ -> raise(Failure("error should have been caught before this"))
        in
        (ty, SMatIndex(v, List.map expr e1))
      | Unop(op, e) as ex ->
        let (t, e') = expr e in
        let ty = match op with
            (* Trans_M when t = Matrix -> t
          | Inv_M when t = Matrix -> t *)
          | Neg when t = Int || t = Float -> t
          | Not when t = Bool -> t
          | _ -> raise (Failure ("illegal unary operator " ^
                                 string_of_uop op ^ " " ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
        in (ty, SUnop(op, (t, e')))
      | Punop(v, op) ->
        let t = type_of_identifier v in
        let ty = match op with
          (* Trans_M when t = Matrix -> t
             | Inv_M when t = Matrix -> t *)
          | Increment when t = Int -> t
          | Decrement when t = Int -> t
          | _ -> raise (Failure ("illegal unary operator " ^
                                 string_of_puop op ^ " " ^ string_of_typ t ^
                                 " in " ^ v))
        in (ty, SPunop(v, op))
      | Binop(e1, op, e2) as e ->
        let (t1, e1') = expr e1
        and (t2, e2') = expr e2 in
        (* All binary operators require operands of the same type *)
        let same = t1 = t2 in
        (* Determine expression type based on operator and operand types *)
        let ty = match op with
            Add | Sub | Mult | Div | Mod when same && t1 = Int   -> Int
          | Add | Sub | Mult | Div       when same && t1 = Float -> Float
          | Add | Sub | Mult             when same && t1 = IntM -> IntM
          (* | Dot_M | Mult_M | Div_M when same && t1 = Matrix -> Matrix *)
          | Eq | Neq                     when same               -> Bool
          | Less | Leq | Greater | Geq
                     when same && (t1 = Int || t1 = Float) -> Bool
          | And | Or when same && t1 = Bool -> Bool
          | _ -> raise (
              Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
        in (ty, SBinop((t1, e1'), op, (t2, e2')))
      | Call(fname, args) as call ->
        let fd = find_func fname in
        let param_length = List.length fd.formals in
        let matching = match args with
          | [] -> ""
          | hd :: _ -> string_of_expr hd
        in
        (* let convert = if List.length args = 1 && test = "" then 0 else List.length args in *)
        let args = if List.length args = 1 && matching = "" then [] else args in
        if List.length args <> param_length then
          raise (Failure ("expecting " ^ string_of_int param_length ^
                          " arguments in " ^ string_of_expr call ^ "\nwhere: args = " ^ string_of_int (List.length args)))
        else let check_call (ft, _) e =
               let (et, e') = expr e in
               let err = "illegal argument found " ^ string_of_typ et ^
                         " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
               in (check_assign ft et err, e')
          in
          let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall(fname, args'))
    in

    let check_bool_expr e =
      let (t', e') = expr e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e')
    in

    let rec check_stmt = function
      | Expr e -> SExpr (expr e)
      | Vdecl(typ, id, e) ->
        let (rt, e') = expr e in
        let err = "illegal assignment of " ^ string_of_typ typ ^  " " ^ id ^
        " to type " ^ string_of_typ rt
        in Hashtbl.add symbols id (check_assign typ rt err) ; SVdecl(typ, id, e')
      | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)
      | For(v, e2, e3, st) ->
          let _ = check_stmt v in

          SFor(check_stmt v, check_bool_expr e2, expr e3, check_stmt st)
      | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)
      | Return e -> let (t, e') = expr e in
        if t = func.typ then SReturn (t, e')
        else raise (
	  Failure ("return gives " ^ string_of_typ t ^ " expected " ^
		   string_of_typ func.typ ^ " in " ^ string_of_expr e))

	    (* A block is correct if each statement is correct and nothing
	       follows any Return statement.  Nested blocks are flattened. *)
      | Block sl ->
          let rec check_stmt_list = function
              [Return _ as s] -> [check_stmt s]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
            | s :: ss         -> check_stmt s :: check_stmt_list ss
            | []              -> []
          in SBlock(check_stmt_list sl)
      | FBlock fl ->
          let rec check_stmt_list = function
              [Return _ as s] -> [check_stmt s]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block fl :: ss  -> check_stmt_list (fl @ ss) (* Flatten blocks *)
            | s :: ss         -> check_stmt s :: check_stmt_list ss
            | []              -> if func.typ <> Void then raise(Failure "Must have a return statement") else []
          in SBlock(check_stmt_list fl)
    in (* body of check_function *)
      { styp = func.typ;
        sfname = func.fname;
        sformals = formals';
        (* slocals = locals'; *)
        sbody = match check_stmt (Block func.body) with
    SBlock(sl) -> sl
        | _ -> let err = "internal error: block didn't become a block?"
        in raise (Failure err)
      }
    in (pname, var_decls', List.map check_function func_decls)
