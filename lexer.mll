
{
  open Parser;;
  exception Lexical_error;; 
}

rule token = parse
    [' ' '\t']  { token lexbuf }
  | "lambda"    { LAMBDA }
  | "L"         { LAMBDA }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "succ"      { SUCC }
  | "pred"      { PRED }
  | "iszero"    { ISZERO }
  | "let"       { LET }
  | "letrec"    { LETREC }
  | "in"        { IN }
  | "Bool"      { BOOL }
  | "Nat"       { NAT }
  | "String"    { STRING }
  | '^'         { STRCAT }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '{'         { LBRACE }
  | '}'         { RBRACE }
  | '['         { LBRACK }
  | ']'         { RBRACK }
  | "nil"       { NIL }
  | "cons"      { CONS }
  | "isnil"     { ISNIL }
  | "head"      { HEAD }
  | "tail"      { TAIL }
  | "List"      { LIST }
  | "unit"      { UNIT }
  | "Unit"      { UNIT_TY }
  | '.'         { DOT }
  | ','         { COMMA }
  | '='         { EQ }
  | ':'         { COLON }
  | ';'         { DOT_COMMA }
  | "->"        { ARROW }
  | ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z']['a'-'z' '_' '0'-'9']*
                { STRINGV (Lexing.lexeme lexbuf) }
  | '"' [^'"']* '"'
                { let s = Lexing.lexeme lexbuf in
                  STRINGL (String.sub s 1 (String.length s - 2))
                }
  | eof         { EOF }
  | _           { raise Lexical_error } 

