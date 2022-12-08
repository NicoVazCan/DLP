
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

let file =  if Array.length Sys.argv > 1 then Sys.argv.(1) else "";;

let inCode = if String.length file > 0 then open_in file else stdin;;

let del = '$';;

let top_level_loop () =
  let rec loop ctxs nl =
    let rec inner_loop cmd nl =
        let line = input_line inCode in
        let cmd' = cmd ^ " " ^ (List.hd (String.split_on_char del line)) in
        if String.contains line del then
          let tm = s token (from_string cmd') in
          loop (execute ctxs tm) (succ nl)
        else inner_loop cmd' (succ nl)
    in
      try inner_loop "" nl with
          Lexical_error ->
            print_endline ("lexical error in line " ^ string_of_int nl);
        | Parse_error ->
            print_endline ("syntax error in line " ^ string_of_int nl);
        | Type_error e ->
            print_endline ("type error in line " ^ string_of_int nl ^ ": " ^ e);
        | End_of_file ->
            ()
  in
    loop (emptyvctx, emptytctx) 1
;;

top_level_loop ()
;;

