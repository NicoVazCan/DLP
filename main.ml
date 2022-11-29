
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

let del = ';';;

let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop ctx =
    let print_term linea = (
      let tm = s token (from_string (linea)) in
            let tyTm = typeof ctx tm in
            print_endline (string_of_term (eval tm) ^ " : " ^ string_of_ty tyTm)) in
    print_string ">> ";
    flush stdout;
    try
      let rec aux s = 
        let linea = read_line() in
        if String.contains linea del
        then print_term (s ^ (List.hd (String.split_on_char del linea)))
        else aux (s ^ linea)
      in aux "";
      loop ctx
    with
       Lexical_error ->
         print_endline "lexical error";
         loop ctx
     | Parse_error ->
         print_endline "syntax error";
         loop ctx
     | Type_error e ->
         print_endline ("type error: " ^ e);
         loop ctx
     | End_of_file ->
         print_endline "...bye!!!"
  in
    loop emptyctx
  ;;

top_level_loop ()
;;

