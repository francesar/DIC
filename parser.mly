
/* Ocamlyacc parser for DIC */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA LBRACK RBRACK COLON
%token PLUS MINUS TIMES TIMES_M DIVIDE DIVIDE_M ASSIGN MOD TRANSPOSE INVERSE DOT
%token INC DEC
%token NOT EQ PEQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR NULL FUNC
%token RETURN IF ELSE FOR WHILE
%token INT BOOL FLOAT VOID LIST DICT STRING
%token <int> LITERAL
%token <bool> BLIT
%token <string> ID FLIT SLIT
%token EOF

%nonassoc NOELSE
%nonassoc ELSE

%right ASSIGN
%left OR
%left AND
%left EQ NEQ PEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD DOT TIMES_M DIVIDE_M
%left INC DEC
%right NOT NEG TRANSPOSE INVERSE

/*%start expr_opt
%type <Ast.expr_opt> expr_opt*/

%start program
%type <Ast.program> program

%%

program:
  | decls EOF { $1 }


decls:
	/* nothing */ { ([], [])					}
  | decls stmt  { ([], [])}
	| decls vdecl { (($2 :: fst $1), snd $1)	}
	| decls fdecl { (fst $1, ($2 :: snd $1))	}

fdecl:
	FUNC typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
		{{ typ = $2;
			fname = $3;
			formals = $5;
			locals = List.rev $8;
			body = List.rev $9}}

formals_opt:
	/* nothing */ { [] }
	| formal_list	{ List.rev $1 }

formal_list:
	typ ID			{ [($1, $2)]				}
	| formal_list COMMA typ ID { ($3, $4) :: $1 }

typ:
	  INT 	{ Int 	}
	| BOOL 	{ Bool 	}
	| FLOAT { Float }
	| VOID 	{ Void 	}
  | STRING { String }

list_type:
  typ LBRACK RBRACK { List($1) }

vdecl_list:
	/* nothing */		{[]}
	| vdecl_list vdecl { $2 :: $1 }

vdecl:
	typ ID SEMI {	($1, $2, Noexpr) }
  | list_type ID SEMI {	($1, $2, Noexpr) }
  | typ ID ASSIGN expr SEMI { ($1, $2, $4) }

stmt_list:
	/* nothing */ { [] }
	| stmt_list stmt { $2 :: $1 }

stmt:
	  expr SEMI												{ Expr $1				  		}
	| RETURN expr_opt SEMI						{ Return $2						}
	| LBRACE stmt_list RBRACE					{ Block(List.rev $2)	}
	| IF LPAREN expr RPAREN stmt %prec NOELSE	{ If($3, $5, Block([])) }
	| IF LPAREN expr RPAREN stmt ELSE stmt 		{ If($3, $5, $7)		}
	| FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
												{ For($3, $5, $7, $9)	}
	| WHILE LPAREN expr RPAREN stmt 			{ While($3, $5)			}

expr_opt:
	/* nothing */ { Noexpr }
	| expr 			{ $1 }

expr:
	  LITERAL          { Literal($1)            }
	| FLIT	     	   	 { Fliteral($1)           }
	| TRUE 						 { BoolLit(true)					}
	| FALSE 					 { BoolLit(false)					}
	| SLIT 			   		 { StringLit($1)					}
	| ID               { Id($1)                 }
  | LBRACK args_opt RBRACK { ListLit($2) }
  | ID LBRACK expr RBRACK { ListIndex ($1, $3) }
  | ID LBRACK expr RBRACK ASSIGN expr { ListIndexAssign ($1, $3, $6) }
	| LBRACK rows	RBRACK { MatLit($2)						}
	| expr PLUS   expr { Binop($1, Add,   $3)   }
	| expr MINUS  expr { Binop($1, Sub,   $3)   }
	| expr TIMES  expr { Binop($1, Mult,  $3)   }
	| expr TIMES_M expr { Binop($1, Mult_M, $3) }
	| expr DOT 	  expr { Binop($1, Dot_M, $3)	}
	| expr DIVIDE expr { Binop($1, Div,   $3)   }
	| expr DIVIDE_M expr { Binop($1, Div_M, $3) }
	| expr MOD 		expr { Binop($1, Mod,   $3) }
	| expr EQ     expr { Binop($1, Eq,    $3)   }
	| expr PEQ 		expr { Binop($1, Peq,   $3) }
	| expr NEQ    expr { Binop($1, Neq,   $3)   }
	| expr LT     expr { Binop($1, Less,  $3)   }
	| expr LEQ    expr { Binop($1, Leq,   $3)   }
	| expr GT     expr { Binop($1, Greater, $3) }
	| expr GEQ    expr { Binop($1, Geq,   $3)   }
	| expr AND    expr { Binop($1, And,   $3)   }
	| expr OR     expr { Binop($1, Or,    $3)   }
	| MINUS expr %prec NEG { Unop(Neg, $2)      }
	| INC expr 			{ Unop(Increment, $2) }
	| DEC expr          { Unop(Decrement, $2) }
	| expr INC 		   { Punop($1, Increment)      }
	| expr DEC 		{ Punop( $1, Decrement)}
	| NOT expr         { Unop(Not, $2)          }
	| TRANSPOSE expr   { Unop(Trans_M, $2)			}
	| INVERSE expr  	 { Unop(Inv_M, $2)        }
	| ID ASSIGN expr   {  Assign($1, $3)        }
	| ID LPAREN args_opt RPAREN { Call($1, $3)  }
	| LPAREN expr RPAREN { $2                   }

args_opt:
	  { [Noexpr] }
	| args_list { List.rev $1 }

args_list:
	expr 							{ [$1] }
	| args_list COMMA expr 			{ $3 :: $1 }

rows:
	args_opt						{ [$1] }
	| rows COLON args_opt 	{$3 :: $1}

/* list_:
  expr { [$1] }
  |  */
