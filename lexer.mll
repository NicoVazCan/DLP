
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
(*The word "letrec" has been added to the lexer in order to identify the recursive expressions in section 2.1.1.
to identify the recursive expressions of section 2.1*)
  | "letrec"    { LETREC }
  | "in"        { IN }
  | "Bool"      { BOOL }
  | "Nat"       { NAT }
(*The word "String" has been added in the lexer in order to
to identify the strings of section 2.3*)
  | "String"    { STRING }
(*The "^" operator has been added to the lexer in order to
to identify the concatenations of section 2.3*)
  | '^'         { STRCAT }
  | '('         { LPAREN }
  | ')'         { RPAREN }
(*Added tokens "LBRACE", "RBRACE", "LBRACK",
"RBRACK", "NIL", "CONS", "ISNIL", "HEAD", "TAIL", "LIST" HAVE BEEN ADDED TO THE LEXER
in the lexer in order to identify the characters of the
lists of section 2.7*)
  | '{'         { LBRACE }
  | '}'         { RBRACE }
(*The tokens "LBRACK", "RBRACK" and "COMMA" have been added in the lexer in order to identify the start and end 
in the lexer in order to identify the start and end, and each member of the pairs, tuples and records of the
member of the pairs, tuples and records in sections 2.4, 2.5 and 2.6.
2.4, 2.5 y 2.6*)
  | '['         { LBRACK }
  | ']'         { RBRACK }
  | "nil"       { NIL }
  | "cons"      { CONS }
  | "isnil"     { ISNIL }
  | "head"      { HEAD }
  | "tail"      { TAIL }
  | "List"      { LIST }
(*The operators "UNIT", "UNIT_TY" and "DOT_COMMA" have been added to the lexer
have been added to the lexer in order to identify the characters in the lists of section 2.9.
lists in section 2.9*)
  | "unit"      { UNIT }
  | "Unit"      { UNIT_TY }
(*The operators "P_NAT", "P_STRING",
"R_NAT", and "R_STRING" have been added in the lexer in order to
identify them*)
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
(*This option has been implemented in the scanner to identify String literals (section 2.3).
String literals (section 2.3) since special characters may appear and have to be
may appear and have to be translated into their corresponding character*)
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

