type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | LBRACK
  | RBRACK
  | COLON
  | PLUS
  | MINUS
  | TIMES
  | TIMES_M
  | DIVIDE
  | DIVIDE_M
  | ASSIGN
  | MOD
  | TRANSPOSE
  | INVERSE
  | CHAN
  | DOT
  | INC
  | DEC
  | NOT
  | EQ
  | PEQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | TRUE
  | FALSE
  | AND
  | OR
  | NULL
  | FUNC
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT
  | BOOL
  | FLOAT
  | VOID
  | LIST
  | DICT
  | STRING
  | LITERAL of (int)
  | BLIT of (bool)
  | ID of (string)
  | FLIT of (string)
  | SLIT of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
