
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

let del = '$';;

let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop ctxs =
    print_string ">> ";
    flush stdout;
    
    let rec inner_loop cmd = print_endline ("loop: " ^ cmd);
      try
        let line = read_line () in
        let cmd' = cmd ^ (List.hd (String.split_on_char del line)) in
        if String.contains line del then
          let tm = s token (from_string cmd') in
          flush stdout; loop (execute ctxs tm)
        else print_string "   "; flush stdout; inner_loop cmd'
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
      inner_loop ""
  in
    loop (emptyvctx, emptytctx)
;;

top_level_loop ()
;;

