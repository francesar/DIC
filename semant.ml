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

  let check_assign lvaluet rvaluet err =
    if lvaluet = rvaluet then lvaluet else raise (Failure err)
  in

  let symbols = List.fold_left
                (fun m (ty, name) -> StringMap.add name ty m)
                StringMap.empty (formals' @ locals')
  in

  let type_of_identifier s =
    try StringMap.find s symbols
    with Not_Found -> raise (Failure ("undeclared identifier " ^ s))
  in

  let rec expr = function
      Literal l         -> (Int, SLiteral l)
    | Cliteral l        -> (String, SCLiteral l)
    | BoolLit l         -> (Bool, SBoolLit l)
    | StringLit l       -> (String, SStringLit l)
    | Noexpr            -> (Void, SNoexpr)
    (* matrix and list stuff: MatLit, MatIndex, MatIndexAssign, ListLit, ListIndex, ListIndexAssign *)
    | Id s              -> (type_of_identifier s, SId s)
    | Binop(e1, op, e2) as e ->
        let (t1, e1') = expr e1
        and (t2, e2') = expr e2 in
        let same = t1 = t2 in
        let ty = match op with
          Add | Sub | Mult | Div | Mod when same && t1 = Int   -> Int
        | Add | Sub | Mult | Div | Mod when same && t1 = Float -> Float
        (* need to add matrix stuff *)
        | Equal | Neq            when same                     -> Bool
        | Less | Leq | Greater | Geq
                   when same && (t1 = Int || t1 = Float) -> Bool
        | And | Or when same && t1 = Bool -> Bool
        | _ -> raise (
      Failure ("illegal binary operator " ^
                     string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                     string_of_typ t2 ^ " in " ^ string_of_expr e))
        in (ty, SBinop((t1, e1'), op, (t2, e2')))
    | Unop(op, e) as ex ->
        let (t, e') = expr e in
        let ty = match op with
          Neg when t = Int || t = Float -> t
        | Not when t = Bool -> Bool
        | _ -> raise (Failure ("illegal unary operator " ^ 
                               string_of_uop op ^ string_of_typ t ^
                               " in " ^ string_of_expr ex))
        in (ty, SUnop(op, (t, e')))
    | Assign(var, e) as ex ->
        let lt = type_of_identifier var
        and (rt, e') = expr e in
        let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
          string_of_typ rt ^ " in " ^ string_of_expr ex
        in (check_assign lt rt err, SAssign(var, (rt, e')))
    | Call(fname, args) as call ->
        let fd = find_func fname in
        let param_length = List.length fd.formals in
        if List.length args != param_length then
          raise (Failure ("expecting " ^ string_of_int param_length ^
                          " arguments in " ^ string_of_expr call))
        else let check_call (ft, _) e =
          let (et, e') = expr e in
          let err = "illegal argument found " ^ string_of_typ et ^
            " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
          in (check_assign ft et err, e')
        in
        let args' = List.map2 check_call fd.formals args
        in (fd.typ, SCall(fname, args'))
  in  *)

  (* lots of code moreee *)
