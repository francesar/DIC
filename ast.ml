(* AST *)

(* Operators *)
(* For now, matrix ops have *_M prefix to denote ops on matrices *)
type op = Add | Sub | Mult | Div  | Eq | Neq | Less |
          Leq | Greater | Geq | And | Or | Mod | Dot | Mult_M | Div_M
          (* Assign *)
          (* Dot_M | *)
          (*| Div_M *)

type uop = Neg | Not | Trans_M
(* Trans_M | Inv_M |  *)
type puop =  Increment | Decrement 

(* Primitive Types *)
type typ =
    Int | Bool | Char | Float | Void | String | IntM | CharM | FloatM | BoolM | StringM | FPoint
  (* | List of typ *)
  (* | Matrix of typ *)

type bind = typ * string

type expr =
    Literal of int
  | Cliteral of string
  | Fliteral of string
  | BoolLit of bool
  | StringLit of string
  | ListLit of expr list
  | ListIndexAssign of string * expr * expr
  | ListIndex of string * expr
  | MatLit of expr list list (* Matrix literal *)
  | MatIndexAssign of string * (expr list) * expr (* Matrix Access Index *)
  | MatIndex of string * (expr list) (* Assign a Matrix Index *)
  | FpointLit of string * (expr list)
  | Id of string
  | Binop of expr * op * expr
  | Punop of string * puop
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr



type var_decl =  typ * string * expr

type stmt =
    Block of stmt list
  | FBlock of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of stmt * expr * expr * stmt
  | While of expr * stmt
  | Vdecl of var_decl

type func_decl = {
  typ: typ;
  fname: string;
  formals: bind list;
  body: stmt list;
}

type program = string * (var_decl list * func_decl list)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Mult_M -> ".*" 
  | Div_M -> ".*" 
  | Div -> "/"
  (* | Div_M -> "./" *)
  (* | Assign -> "=" *)
  | Eq -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | Mod -> "%"
  | Dot -> "**" 

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"
  | Trans_M -> "'"
     (* | Inv_M -> "~" *)
let string_of_puop = function
    Increment -> "++"
  | Decrement -> "--"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Cliteral(c) -> c
  | Fliteral(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | StringLit(s) -> s
  | ListLit(l) -> "[" ^ String.concat ", " (List.map string_of_expr l) ^ "]"
  | ListIndexAssign(v,e1,e2) -> v ^ "[" ^ string_of_expr e1 ^ "] = " ^ string_of_expr e2
  | ListIndex(v, e1) -> v ^ "[" ^ string_of_expr e1 ^ "]"
  | MatLit(rows) ->
      "[" ^
      let rec print_list input_list = match (List.rev input_list) with
      | [s] -> s
      | [] -> ""
      | e :: l -> e ^ ":" ^ print_list (List.rev l) in
      print_list (List.map ( let rec print_row = function
        | [s] -> string_of_expr s
        | [] -> ""
        | h :: t -> string_of_expr h ^ "," ^ print_row t in
      fun anon -> print_row anon) rows)
      ^ "]"
  | MatIndexAssign (v, e1, e2) -> v ^ "[" ^ String.concat "][" (List.map string_of_expr e1) ^ "] = " ^ string_of_expr e2
  | MatIndex (v, e1) -> v ^ "[" ^ String.concat "][" (List.map string_of_expr e1) ^ "]"
  | FpointLit (s, l) -> "<" ^ s ^ ">" ^  "[" ^ String.concat "," (List.map string_of_expr l) ^ "]"
  | Id(s) -> s
  | Binop(e1, o , e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Punop(v, o) ->  v ^ string_of_puop o
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ (String.concat ", " (List.map string_of_expr el)) ^ ")"
  | Noexpr -> ""

let string_of_typ = function
    Int -> "int"
  | Char -> "char"
  | String -> "string"
  | Bool -> "bool"
  | Float -> "float"
  | Void -> "void"
  | IntM -> "int[]"
  | CharM -> "char[]"
  | StringM -> "string[]"
  | BoolM -> "bool[]"
  | FloatM -> "float[]"
  | FPoint -> "fpoint"
  (* | List(t) -> string_of_typ t ^ "[]"
  | Matrix(t) -> string_of_typ t ^ "[][]" *)

let string_of_vdecl = function
  | (t, id, exp) ->
    if exp = Noexpr then string_of_typ t ^ " " ^ id ^ ";"
    else string_of_typ t ^ " " ^ id ^ " = " ^ string_of_expr exp ^ ";"

let rec string_of_stmt = function
  | Vdecl(vdecls) ->
      string_of_vdecl vdecls ^ "\n"
  | Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | FBlock(stmts) -> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ "; \n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) -> "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For( v1, e2, e3, s) ->
      "for (" ^ string_of_stmt v1 ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3 ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_vdecl = function
  | (t, id, exp) ->
      if exp = Noexpr then string_of_typ t ^ " " ^ id ^ ";\n"
      else string_of_typ t ^ " " ^ id ^ " = " ^ string_of_expr exp ^ ";\n"

let string_of_binding = function
  | (t, id) -> string_of_typ t ^ " " ^ id ^ ""

let string_of_fdecl fdecl =
  "func " ^ string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_binding fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (name, (vars, funcs)) =
  "class " ^ name ^ " {" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs) ^
  "}\n"
