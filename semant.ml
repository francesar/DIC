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

  (* Raise an exception if the given rvalue type cannot be assigned to
      the given lvalue type *)
    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
                  StringMap.empty (globals' @ formals')
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
      | SMatLit m   -> (Matrix, SMatLit m)
      | SListLit l  -> (List, SListLit l)
      | Noexpr      -> (Void, SNoexpr)
      | Id s        -> (type_of_identifier s, SId s)
      | Assign(var, e) as ex ->
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
            Add | Sub | Mult | Div | Mod when same && t1 = Int   -> Int
          | Add | Sub | Mult | Div when same && t1 = Float -> Float
          | Dot_M | Mult_M | Div_M when same && t1 = Matrix -> Matrix
          | Eq | Neq            when same               -> Bool
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
in



let check_bool_expr e =
  let (t', e') = expr e
  and err = "expected Boolean expression in " ^ string_of_expr e
  in if t' != Bool then raise (Failure err) else (t', e')
in

(* Return a semantically-checked statement i.e. containing sexprs *)
let rec check_stmt = function
    Expr e -> SExpr (expr e)
  | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)
  | For(e1, e2, e3, st) ->
    SFor(expr e1, check_bool_expr e2, expr e3, check_stmt st)
  | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)
  | Return e -> let (t, e') = expr e in
    if t = func.typ then SReturn (t, e')
    else raise (
        Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                 string_of_typ func.typ ^ " in " ^ string_of_expr e))
  (* Check bindings and add to symbol table *)
  | VDecl(ty, s, e) -> let (t,e') = expr e in
    if t = ty then let local_bind = [(ty, s)] in
    let local' = check_binds "local" local_bind in
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
        symbols (local') in  SVdecl(ty,s,e')
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

in (* body of check_function *)
{ styp = func.typ;
  sfname = func.fname;
  sformals = formals';
  slocals  = locals';
  sbody = match check_stmt (Block func.body) with
      SBlock(sl) -> sl
    | _ -> let err = "internal error: block didn't become a block?"
      in raise (Failure err)
}
in (globals', List.map check_function functions)
