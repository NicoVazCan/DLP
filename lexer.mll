
{
  open Parser;;
  exception Lexical_error;; 

  let string_buff = Buffer.create 256;;

  let list_of_string s = List.init (String.length s) (String.get s)
  ;;
  let isnat s = List.for_all (String.contains "0123456789") (list_of_string s)
  ;;

  let char_for_backslash = function
    | "n" -> '\n'
    | "r" -> '\r'
    | "b" -> '\b'
    | "t" -> '\t'
    | c when isnat c -> char_of_int (int_of_string c)
    | c   -> c.[0]
  ;;
}

rule token = parse
    [' ' '\t' '\n'] | "(*" _?('*'[^')'] | [^'*']_)* "*)" 
                { token lexbuf }
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
(*Se ha añadido la palabra "letrec" en el lexer con el fin
de identificar las expresiones recursivas del apartado 2.1*)
  | "letrec"    { LETREC }
  | "in"        { IN }
  | "Bool"      { BOOL }
  | "Nat"       { NAT }
(*Se ha añadido la palabra "String" en el lexer con el fin
de identificar los strings del apartado 2.3*)
  | "String"    { STRING }
(*Se ha añadido el operador "^" en el lexer con el fin
de identificar las concatenaciones del apartado 2.3*)
  | '^'         { STRCAT }
  | '('         { LPAREN }
  | ')'         { RPAREN }
(*Se han añadido los operadores "LBRACE", "RBRACE", "LBRACK",
"RBRACK", "NIL", "CONS", "ISNIL", "HEAD", "TAIL", "LIST"
en el lexer con el fin de identificar los caracteres de las
listas del apartado 2.7*)
  | '{'         { LBRACE }
  | '}'         { RBRACE }
(*Se han añadido los operadores "LBRACK", "RBRACK" y "COMMA" 
en el lexer con el fin de identificar los caracteres de los
pares, tuplas y registros de los apartados 2.4, 2.5 y 2.6*)
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
  | "print_nat" { P_NAT }
  | "print_string"
                { P_STRING }
  | "read_nat"  { R_NAT }
  | "read_string"
                { R_STRING }
  | '.'         { DOT }
  | ','         { COMMA }
  | '='         { EQ }
  | ':'         { COLON }
  | ';'         { DOT_COMMA }
  | "->"        { ARROW }
  | ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z']['a'-'z' '_' '0'-'9']*
                { STRINGV (Lexing.lexeme lexbuf) }
(*Se ha implementado esta opción en el escáner para identificar
literales de String (apartado 2.3) ya que pueden aparecer caracteres especiales
y tienen que ser traducidos a su caracter correspondiente*)
  | '"'
                { Buffer.clear string_buff;
                  string lexbuf;
                  STRINGL (Buffer.contents string_buff)
                }
  | eof         { EOF }
  | _           { raise Lexical_error }

and string = parse
  | '"'
      { () }
  | '\\' (['\\' '\'' '\"' 'n' 't' 'b' 'r' ' ']|'0'['0'-'9']+ as c)
      { Buffer.add_char string_buff (char_for_backslash c);
        string lexbuf }
  | _ as c
      { Buffer.add_char string_buff c;
        string lexbuf }

