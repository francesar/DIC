(* AST *)

(* Operators *)
(* For now, matrix ops have *_M prefix to denote ops on matrices *)
type op = Add | Sub | Mult | Div | Assign | Eq | Peq | Neq | Less |
          Leq | Greater | Geq | And | Or | Mod | Dot_M | Chan


type uop = Neg | Not | Trans_M | Inv_M

(* Primitive Types *)
type typ = Int | Bool | Char | Float | Null

(* Data Types *)
type datatyp = String | List | Dict | Matrix

type bind = typ * string

type expr =
    Literal of int
  | Fliteral of string
  | BoolLit of bool
  | StringLit of string
  | MatLit of expr list list (* Matrix literal *)
  | MatIndex of string * expr * expr (* Matrix Access Index *)
  | MatIndexAssign of string * expr * expr * expr (* Assign a Matrix Index *)
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

type stmt =
    Block of stmt list
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
