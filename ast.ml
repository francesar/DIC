(* AST *)

(* Operators *)
(* For now, matrix ops have *_M prefix to denote ops on matrices *)
type op = Add | Sub | Mult | Div | Assign | Eq | Peq | Neq | Less |
          Leq | Greater | Geq | And | Or | Mod | Dot_M |
          Mult_M | Div_M | Chan



type uop = Neg | Not | Trans_M | Inv_M | Increment | Decrement

(* Primitive Types *)
type typ =
    Int | Bool | Char | Float | Void | String
  | List of typ
  | Matrix of typ

(* Data Types
   type datatyp = String | List | Dict | Matrix *)

type bind = typ * string

type expr =
    Literal of int
  | Fliteral of string
  | BoolLit of bool
  | StringLit of string
  | MatLit of expr list list (* Matrix literal *)
  | MatIndex of string * expr * expr (* Matrix Access Index *)
  | MatIndexAssign of string * expr * expr * expr (* Assign a Matrix Index *)
  | ListLit of expr list
  | ListIndex of string * expr
  | ListIndexAssign of string * expr * expr
  | Id of string
  | Binop of expr * op * expr
  | Punop of expr * uop
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

type var_decl =  typ * string * expr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type func_decl = {
  typ: typ;
  fname: string;
  formals: bind list;
  locals: var_decl list;
  body: stmt list;
}

(* type class_decl = {
  cname: string;
  vbody: func_decl list;
} *)

type program = var_decl list * func_decl list

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Mult_M -> ".*"
  | Div -> "/"
  | Div_M -> "./"
  | Assign -> "="
  | Eq -> "=="
  | Peq -> "==="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | Mod -> "%"
  | Dot_M -> "**"
  | Chan -> "<~"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"
  | Trans_M -> "'"
  | Inv_M -> "~"
  | Increment -> "++"
  | Decrement -> "--"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Fliteral(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | StringLit(s) -> s
  (* | MatLit(rows) -> "[" ^ rows ^ "]" *)
  | Id(s) -> s
  | Binop(e1, o , e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Punop(e, o) ->  string_of_expr e ^ string_of_uop o 
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ "; \n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) -> "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1 ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3 ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Void -> "void"

let string_of_vdecl = function 
  | (t, id, exp) -> string_of_typ t ^ " " ^ id ^ " = " ^ string_of_expr exp ^ ";\n"

let string_of_binding = function 
  | (t, id) -> string_of_typ t ^ " " ^ id ^ ""

let string_of_fdecl fdecl =
  "func " ^ string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_binding fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
