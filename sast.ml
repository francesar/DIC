open Ast

type sexpr = typ * sx 
and sx = 
  | SLit of int 
  | SCLit of string 
  | SFLit of string 
  | SBoolLit of bool 
  | SStringLit of string 
  | SMatLit of sexpr list list
  | SMatIndex of string * sexpr * sexpr
  | SMatIndexAssign of string * sexpr * sexpr * sexpr
  | SListLit of sexpr list 
  | SListIndex of string * sexpr 
  | SListIndexAssign of string * sexpr * sexpr
  | SId of string 
  | SBinop of sexpr * op * sexpr 
  | SPunop of sexpr * uop
  | SUnop of uop * sexpr
  | SAssign of string * sexpr 
  | SCall of string * sexpr list 
  | SNoExpr 

type svar_decl = typ * string * sexpr

type sstmt = 
  | SBlock of sstmt list 
  | SExpr of sexpr 
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of svar_decl * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt

(* Unsure why some of these are semantically checked and not... just following microC for now*)
type sfunc_decl = {
  styp: typ;
  sfname: string;
  sformals: bind list;
  slocals: svar_decl list;
  body: sstmt list;
}

type sprogram = string * (svar_decl * sfunc_decl list) 

