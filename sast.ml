open Ast

type sexpr = typ * sx
and sx =
  | SLit of int
  | SCLit of string
  | SFLit of string
  | SBoolLit of bool
  | SStringLit of string
  | SListLit of sexpr list
  (* | SMatLit of sexpr list list
  | SMatIndex of string * sexpr * sexpr
  | SMatIndexAssign of string * sexpr * sexpr * sexpr
  | SListLit of sexpr list
  | SListIndex of string * sexpr
  | SListIndexAssign of string * sexpr * sexpr *)
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SPunop of sexpr * puop
  | SUnop of uop * sexpr
  | SAssign of string * sexpr
  | SCall of string * sexpr list
  | SNoExpr

type svar_decl = typ * string * sx

type sstmt =
  | SBlock of sstmt list
  | SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sstmt * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SVdecl of svar_decl

type sfunc_decl = {
  styp: typ;
  sfname: string;
  sformals: bind list;
  (* slocals: svar_decl list; *)
  sbody: sstmt list;
}

type sprogram = string * (svar_decl list * sfunc_decl list)

(* Pretty printing functions *)

let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    SLit(l) -> string_of_int l
  | SCLit(c) -> c
  | SFLit(l) -> l
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SStringLit(s) -> s
  | SId(s) -> s
  | SBinop(e1, o , e2) ->
    string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr(e)
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SPunop(e, o) ->  string_of_sexpr e ^ string_of_puop o
  (*| SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e*)
  | SCall(f, el) ->
    f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SNoExpr -> ""
  | SListLit(el) -> "[" ^ String.concat ", " (List.map string_of_sexpr el) ^ "]"
  (*| SListIndex(v,e) -> v ^ "[" ^ string_of_sexpr e ^ "]"
  | SListIndexAssign(v,e1,e2) -> v ^ "[" ^ string_of_sexpr e1 ^ "] = " ^ string_of_sexpr e2
  | SMatLit(rows) ->
    "[" ^
    let rec print_list input_list = match (List.rev input_list) with
      | [s] -> s
      | [] -> ""
      | e :: l -> e ^ ":" ^ print_list (List.rev l) in
    print_list (List.map ( let rec print_row = function
        | [s] -> string_of_sexpr s
        | [] -> ""
        | h :: t -> string_of_sexpr h ^ "," ^ print_row t in
        fun anon -> print_row anon) rows)
    ^ "]"
  | SMatIndex (v, e1, e2) -> v ^ "[" ^ string_of_sexpr e1 ^ "]" ^ "[" ^ string_of_sexpr e2 ^ "]"
  | SMatIndexAssign (v, e1, e2, e3) ->
    v ^ "[" ^ string_of_sexpr e1 ^ "]" ^ "[" ^ string_of_sexpr e2 ^ "] = " ^ string_of_sexpr e3*)
  ) ^ ")"

(* Awkard dangling = if exp is NoExpr *)
let string_of_svdecl = function
  | (t, id, exp) -> string_of_typ t ^ " " ^ id ^ " = " ^ string_of_sexpr (t, exp) ^ ";"

let rec string_of_sstmt = function
    SBlock(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n";
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ "; \n";
  | SIf(e, s, SBlock([])) -> "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
  | SIf(e, s1, s2) -> "if (" ^ string_of_sexpr e ^ ")\n" ^
                     string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SFor( v1, e2, e3, s) ->
    "for (" ^ string_of_sstmt v1 ^ string_of_sexpr e2 ^ " ; " ^
    string_of_sexpr e3 ^ ") " ^ string_of_sstmt s
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SVdecl (t, id, exp) -> string_of_typ t ^ " " ^ id ^ " = " ^ string_of_sexpr (t, exp) ^ ";\n"

let string_of_sfdecl fdecl =
  "func " ^ string_of_typ fdecl.styp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map string_of_binding fdecl.sformals) ^
  ")\n{\n" ^
  (* String.concat "" (List.map string_of_svdecl fdecl.slocals) ^ "\n" ^ *)
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"


let string_of_sprogram (name, _, funcs) =
  "class " ^ name ^ " {" ^
  (* String.concat "" (List.map string_of_binding vars) ^ "\n" ^ *)
  String.concat "\n" (List.map string_of_sfdecl funcs) ^
  "}\n"
  (* not converting list of bindings to list of svdecl *)
