/* Ocamlyacc parser for DIC */

%{
open Ast
%}


%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA APOS LBRACK RBRACK BAR
%token PLUS MINUS TIMES DIVIDE ASSIGN MOD DOT MATTRANS 
%token NOT EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR NULL
%token RETURN IF ELSE FOR WHILE 
%token INT BOOL FLOAT VOID STRING MATRIX 
%token <int> LITERAL
%token <bool> BLIT
%token <string> ID FLIT SLIT
%token EOF

%nonassoc NOELSE
%nonassoc ELSE

%right ASSIGN
%left OR
%left AND 
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD DOT
%right NOT NEG
%left MATTRANS

%start program
%type <Ast.program> program

%%

program:
	decls EOF { $1 }