(* Ocamllex scanner for MicroC *)

{ open Parser }

let digit = ['0' - '9']
let digits = digit+

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| "//"     { inline lexbuf  }           (* Inline*)
(* Syntax *)
| '"'	   { QUOTE		 }
| '('      { LPAREN      }
| ')'      { RPAREN      }
| '{'      { LBRACE      }
| '}'      { RBRACE      }
| '['      { LBRACK      }
| ']'      { RBRACK      }
| ':'      { COLON       }
| ';'      { SEMI        }
| ','      { COMMA       }
(* Operators *)
(* | '''      { TRANSPOSE   } *)
| '+'      { PLUS        }
| "++"     { INC         }
| "--"     { DEC         }
| '-'      { MINUS       }
| '*'      { TIMES       }
(* | ".*"     { TIMES_M     } *)
(* | "./"     { DIVIDE_M    } *)
(* | "**"     { DOT         } *)
(* | '~'      { INVERSE     } *)
| '/'      { DIVIDE      }
| '%'      { MOD         }
| '='      { ASSIGN      }
| "=="     { EQ          }
(* | "==="    { PEQ         } *)
| "!="     { NEQ         }
| '<'      { LT          }
| "<="     { LEQ         }
| ">"      { GT          }
| ">="     { GEQ         }
| "&&"     { AND         }
| "||"     { OR          }
| "!"      { NOT         }
(* Control *)
| "class"  { CLASS       }
| "func"   { FUNC        }
| "if"     { IF          }
| "else"   { ELSE        }
| "for"    { FOR         }
| "while"  { WHILE       }
| "return" { RETURN      }
(* Types *)
| "int"    { INT         }
| "int[]"  { INTM		 }
| "float[]" { FLOATM	 }
| "char[]" { CHARM		 }
| "string[]" { STRINGM	 }
| "bool[]" { BOOLM 		 }
(*| "int[" digits "]" ('[' digits ']')* as lxm { INTM(lxm) }*)
| "char"   { CHAR        }
| "bool"   { BOOL        }
| "float"  { FLOAT       }
| "void"   { VOID        }
| "list"   { LIST        }
| "string" { STRING      }
(* Literals *)
| "true"   { TRUE  }
| "false"  { FALSE }
| digits as lxm { LITERAL(int_of_string lxm) }
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { FLIT(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { ID(lxm) }
| '"'[^'"']* '"' as lxm {SLIT(String.sub lxm 1 ((String.length lxm )- 2))}
| ''' ['a'-'z' 'A'-'Z' '0'-'9' '_'] ''' as lxm {CHLIT(lxm)}
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf   }
| _    { comment lexbuf }

and inline = parse
  '\n' { token lexbuf   }
| _    { inline lexbuf  }
