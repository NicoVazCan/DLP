
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

