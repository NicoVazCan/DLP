
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

(*
For the implementation of section 1.1 the file has received the following changes:
- The variable "del" with the value "4" has been added, whose function is to indicate the end of
  of an expression so that the rest of the input is not processed if there is any.
- The recursive function "inner_loop" has been implemented.
  until it finds the character that corresponds to the variable "del".
*)

(*The implementation of the global context was achieved by adding:
		-Two axiomatic expressions to the grammar; one for definition, in which the entered term is first evaluated and then the resultant is assigned to the chosen variable; and one for evaluation, which only evaluates the term.
		-Another context to store the types of the defined global variables, apart from the value context, so as not to have to infer it every time a global variable is mentioned.
		-A function called 'execute' in 'lambda.ml' that, depending on the expression chosen between the two previously mentioned, adds to the contexts the type and value corresponding to the defined variable and then returns them, apart from evaluating and inferring the type of the term.
		-A function called 'apply_ctx' in 'lambda.ml' that, after fully evaluating the term delivered to one of the two previously mentioned expressions, the free variables of this one are replaced by their corresponding value in the value context.
		-The 'eval' and 'eval1' functions of 'lambda.ml' were modified to contain the value context and to be able to return the value corresponding to the global variables that appear in the term given to it.
		-Finally, the 'loop' function in 'main.ml' was modified to only apply the new 'execute' function.
	The reasons why it was decided to implement the global context in this way were:
		-Simplicity; by keeping the type of each global variable in a separate type context from the value context, it makes the implementation and performance task easier, albeit at the cost of memory by storing the variable names twice for each context to access their value or type.
		-It was the first one we understood, so, for lack of time, we decided to continue with this implementation.*)

let del = '$';;

let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop ctxs =
    print_string ">> ";
    flush stdout;
    
    let rec inner_loop cmd =
        let line = read_line () in
        let cmd' = cmd ^ "\n" ^ (List.hd (String.split_on_char del line)) in
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

