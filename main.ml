
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

%
Para la implementación del apartado 1.1 el archivo ha recibido los siguientes cambios:
- Se ha añadido la variable "del" con el valor "4", cuya función es indicar el final
  de una expresión para que no se procese el resto de entrada en caso de haberla.
- Se ha implementado la función recursiva "inner_loop", que va analizando la entrada
  hasta encontrar al caracter que se corresponde con la variable "del"
%

let del = '$';;

let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop ctxs =
    print_string ">> ";
    flush stdout;
    
    let rec inner_loop cmd =
        let line = read_line () in
        let cmd' = cmd ^ " " ^ (List.hd (String.split_on_char del line)) in
        if String.contains line del then
          let tm = s token (from_string cmd') in
          loop (executeAndPrint ctxs tm)
        else print_string "   "; flush stdout; inner_loop cmd'
    in
      try inner_loop "" with
          Lexical_error ->
            print_endline "lexical error";
            loop ctxs
        | Parse_error ->
            print_endline "syntax error";
            loop ctxs
        | Type_error e ->
            print_endline ("type error: " ^ e);
            loop ctxs
        | End_of_file ->
            print_endline "...bye!!!";
            exit 0
  in
    loop (emptyvctx, emptytctx)
;;

top_level_loop ()
;;

