(* AST *)

(* Operators *)
(* For now, matrix ops have *_M prefix to denote ops on matrices *)
type op = Add | Sub | Mult | Div | Equal | Neq | Less |
          Leq | Greater | Geq | And | Or | Mod | CmpStruct |
          CmpPhys |
          Add_M | Sub_M | Mult_M | Dot_M | 

type uop = Neg | Not | Trans_M

(* Primitive Types *)
type typ = Int | Bool | Char | Float | Double | Null

type bind = typ * string

type expr = 
  | Literal of int 
  | Fliteral of string 
  | BoolLit of bool
  | Id of string 
  | Binop of expr * op * exp
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

type stmt = 
  | Block of stmt list 
  | Expr of expr 
  | Return of expr 
  | If of expr * stmt * expr 
  | For of expr * expr * expr * stmt
  | While of expr * stmt 
  
type func_decl = {
  typ: typ;
  fname: string;
  formals: bind list;
  locals: bind list;
  body: stmt list;
}

type program = bind list * func_decl list