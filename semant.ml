open Ast
open Sast
module StringMap = Map.Make(String)

type func_symbol = func_decl StringMap.t

(* args here might need to change since we accept classes OR statment blocks as valid programs *)
let check (class, functions) =

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

  (* Raise an exception if the given rvalue type cannot be assigned to
      the given lvalue type *)
    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
                  StringMap.empty (globals' @ formals' @ locals' )
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return a semantically-checked expression*)
    let rec expr = function
        Literal l   -> (Int, SLiteral l)
      | Cliteral l  -> (Char, SCliteral l)
      | Fliteral l  -> (Float, SFliteral l)
      | BoolLit l   -> (Bool, SBoolLit l)
      | StringLit s -> (String, SStringLit s)
      | Noexpr      -> (Void, SNoexpr)
      | Id s        -> (type_of_identifier s, SId s)
      | Assign(var, e) as ex ->
        (* Need to change eventually to allow assignment anywhere*)
        let lt = type_of_identifier var
        and (rt, e') = expr e in
        let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
          string_of_typ rt ^ " in " ^ string_of_expr ex
        in (check_assign lt rt err, SAssign(var, (rt, e')))
      | Punop(op, e) as ex ->
        let (t, e') = expr e in
        let ty = match op with
            Neg when t = Int || t = Float -> t
          | Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary operator" ^
                                 string_of_uop ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
        in (ty, SPunop(op, (t, e')))
      | Unop(e, op) as ex ->
        let (t, e') = expr e in
        let ty = match op with
            Trans_M when t = Matrix -> t
          | Inv_M when t = Matrix -> t
          | Increment when t = Int || t = Float -> t
          | Decrement when t = Int || t = Float -> t
          | _ -> raise (Failure ("illegal unary operator" ^
                                 string_of_uop ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
        in (ty, Sunop(op, (t, e')))
      | Binop(e1, op, e2) as e ->
        let (t1, e1') = expr e1
        and (t2, e2') = expr e2 in
        (* All binary operators require operands of the same type *)
        let same = t1 = t2 in
        (* Determine expression type based on operator and operand types *)
        let ty = match op with
            Add | Sub | Mult | Div when same && t1 = Int   -> Int
          | Add | Sub | Mult | Div when same && t1 = Float -> Float
          | Eq | Neq            when same               -> Bool
          | Less | Leq | Greater | Geq
            when same && (t1 = Int || t1 = Float) -> Bool
          | And | Or when same && t1 = Bool -> Bool
          | _ -> raise (
              Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
        in (ty, SBinop((t1, e1'), op, (t2, e2')))
