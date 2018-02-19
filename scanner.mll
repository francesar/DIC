(* Ocamllex scanner for MicroC *)

{ open Parser }

let digit = ['0' - '9']
let digits = digit+

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| "//"     { inline lexbuf }            (* Inline*)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACK }
| ']'      { RBRACK }
| ':'      { COLON }
| ';'      { SEMI }
| ','      { COMMA }
(* OPERATORS *)
| '''      { TRANSPOSE }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| ".*"		{ MULT_M }
| "./"		{ DIV_M }
| "**"	   { DOT }
| '~'	     { INVERSE }
| '/'      { DIVIDE }
| '%'      { MOD }
| '='      { ASSIGN }
| "=="     { EQ }
| "==="    { PEQ}
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "<~"     { CHAN }
(* CONTROL *)
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "bool"   { BOOL }
| "float"  { FLOAT }
| "void"   { VOID }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| "func"   { FUNC }
| digits as lxm { LITERAL(int_of_string lxm) }
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { FLIT(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and inline = parse
  '\n' { token lexbuf }
| _    { inline lexbuf }
