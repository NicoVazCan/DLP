
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop ctxs =
    print_string ">> ";
    flush stdout;
    try
      let c = s token (from_string (read_line ())) in 
      loop (execute ctxs c)
    with
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
         print_endline "...bye!!!"
  in
    loop (emptyvctx, emptytctx)
  ;;

top_level_loop ()
;;

