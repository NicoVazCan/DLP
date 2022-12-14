
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

(*
Para la implementación del apartado 1.1 el archivo ha recibido los siguientes cambios:
- Se ha añadido la variable "del" con el valor "4", cuya función es indicar el final
  de una expresión para que no se procese el resto de entrada en caso de haberla.
- Se ha implementado la función recursiva "inner_loop", que va analizando la entrada
  hasta encontrar al caracter que se corresponde con la variable "del"
*)

(*Explicar la implementación del contexto global y porque se hizo así:
	La implementación del contexto global se consiguió mediante la adición:
		·Dos expresiones axiomáticas a la gramática; una de definición, en la que primero se evalúa el término introducido y luego se le asigna el resultante a la variable escogida; y otra de evaluación, que unicamente evalúa el término.
		·Otro contexto para almacenar los tipos de las variables globales definidas, aparte del contexto de valores, y así no tener que inferirlo cada vez que se menciona alguna variable global.
		·Una función llamada 'execute' en 'lambda.ml' que, dependiendo de la expresión escogida entre las dos previamente mencionadas, se añade a los contextos el tipo y valor correspondiente a la variable definida para luego devolverlos, aparte de evaluar e inferir el tipo del término.
		·Una función llamada 'apply_ctx' en 'lambda.ml' que, después de evaluar completamente el término entregado a alguna de las dos expresiones previamente mencionadas, se sustituyen las variables libres de este por su valor correspondiente en el contexto de valores.
		·Se modificó las funciones 'eval' y 'eval1' de 'lambda.ml' para contener el contexto de valores y poder devolver el valor corespondiente a las variables globales que aparezcan en el término que se le entregue.
		·Finalmente se modificó la función 'loop' en 'main.ml' para únicamente aplicar la nueva función 'execute'.
	Las razones por las que se decidió implementar el contexto global de esta manera fueron:
		·Sencillez; al mantener el tipo de cada variable global en un contexto de tipos separado del de valores, facilita la tarea de implementación y de rendimiento, aunque a costa de la memoria al guardar los nombres de las variables dos veces para cada contexto y así poder acceder su valor o su tipo.
		·Fue el primero que entendimos, por lo que, a falta de tiempo, decidimos continuar con esta implementación.*)

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

