
open Format;;

(* TYPE DEFINITIONS *)

type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyStr
  | TyRcd of (string * ty) list
  | TyList of ty
  | TyUnit
;;

type tcontext =
  (string * ty) list
;;

type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLetIn of string * term * term
  | TmFix of term
  | TmStr of string
  | TmStrCat of term * term
  | TmRcd of (string * term) list
  | TmProj of term * string
  | TmNil of ty
  | TmCons of ty * term * term
  | TmIsNil of ty * term
  | TmHead of ty * term
  | TmTail of ty * term
  | TmUnit
  | TmPrtNat of term
  | TmPrtStr of term
  | TmRdNat of term
  | TmRdStr of term
;;

type vcontext =
  (string * term) list
;;

type command = 
    Ignore
  | Eval of term
  | Bind of string * term
;;



(* CONTEXT MANAGEMENT *)

let emptytctx =
  []
;;

let addtbinding tctx x bind =
  (x, bind) :: tctx
;;

let gettbinding tctx x =
  List.assoc x tctx
;;


let emptyvctx =
  []
;;

let addvbinding vctx x bind =
  (x, bind) :: vctx
;;

let getvbinding vctx x =
  List.assoc x vctx
;;



(* TYPE MANAGEMENT (TYPING) *)

let list_of_string s = List.init (String.length s) (String.get s)
;;

let isnat s = List.for_all (String.contains "0123456789") (list_of_string s)
;;

let rec string_of_ty ty = match ty with
    TyBool ->
      "Bool"
  | TyNat ->
      "Nat"
  | TyArr (ty1, ty2) ->
      "(" ^ string_of_ty ty1 ^ ")" ^ " -> " ^ "(" ^ string_of_ty ty2 ^ ")"
  | TyStr ->
      "String"
(*Se ha añadido la capacidad de identificar tuplas y registros a la 
función string_of_ty para el desarrollo de los apartados 2.5 y 2.6*)
  | TyRcd [] ->
      "{}"
  | TyRcd tyL when isnat (fst (List.hd tyL)) ->
      let sFdL = List.map (fun (_, ty) -> string_of_ty ty) tyL in
      "{" ^ (String.concat ", " sFdL) ^ "}"
  | TyRcd tyL ->
      let sFdL = List.map (fun (fn, ty) -> fn ^ ":" ^ string_of_ty ty) tyL in
      "{" ^ (String.concat ", " sFdL) ^ "}"
(*Se ha añadido la capacidad de identificar listas a la 
función string_of_ty para el desarrollo del apartado 2.7*)
  | TyList ty ->
      "List " ^ (string_of_ty ty)
  | TyUnit ->
      "Unit"  
;;

let rec print_ty = function
    TyArr (ty1, ty2) ->
      open_box 1;
        print_atomicTy ty1;
      close_box ();
      print_string " ->";
      print_space ();
      open_box 1;
        print_ty ty2;
      close_box ();
      ()
  | TyList ty ->
      print_string "List ";
      open_box 1;
        print_ty ty;
      close_box ();
      ()
  | ty ->
      print_atomicTy ty;
and print_atomicTy = function
    TyBool ->
      print_string "Bool";
      ()
  | TyNat ->
      print_string "Nat";
      ()
  | TyRcd [] ->
      print_string "{}";
      ()
  | TyRcd fdTyL when isnat (fst (List.hd fdTyL)) ->
      let rec aux = function
          [] ->
            ()
        | h::[] ->
            print_ty h;
            ()
        | h::t ->
            print_ty h;
            print_char ',';
            print_space ();
            aux t;
      in
        print_char '{';
        open_box 1;
          aux (snd (List.split fdTyL));
        close_box ();
        print_char '}';
        ()
  | TyRcd fdTyL ->
      let rec aux = function
          [] ->
            ()
        | (fn,ty)::[] ->
            print_string fn;
            print_char '=';
            print_ty ty;
            ()
        | (fn,ty)::t ->
            print_string fn;
            print_char '=';
            print_ty ty;
            print_char ',';
            print_space ();
            aux t;
      in
        print_char '{';
        open_box 1;
          aux fdTyL;
        close_box ();
        print_char '}';
        ()
  | TyUnit ->
      print_string "Unit";
      ()
  | ty ->
      print_char '(';
      open_box 1;
        print_ty ty;
      close_box ();
      print_char ')';
      ()
;;

exception Type_error of string
;;

(*Para implementar el apartado 2.8 se creo la función interfija
'<:' que se usará en 'typeof' para comprobar si un tipo es subtipo
de otro. La función consiste en la aplicación recursiva de las reglas
de subtipado de abstracciones y registros.*)
let rec typeof tctx tm = 
  let rec (<:) s t = match s, t with
        (* Basic typing rule *)
      _ when s = t ->
        true

        (* S-Arrow *)
    | TyArr (s1, s2), TyArr (t1, t2) when
          t1 <: s1 && s2 <: t2 ->
        true

        (* S-RcdWidth/S-RcdDepth/S-RcdPerm *)
    | TyRcd sFdTyL, TyRcd tFdTyL when
        not (sFdTyL != [] && (isnat (fst (List.hd sFdTyL)))) &&
        List.for_all (fun (nm, ty) ->
          try (List.assoc nm sFdTyL) <: ty with 
            Not_found -> false) tFdTyL
         ->
        true

    | _ ->
        false
  in match tm with
    (* T-True *)
    TmTrue ->
      TyBool

    (* T-False *)
  | TmFalse ->
      TyBool

    (* T-If *)
  | TmIf (t1, t2, t3) ->
      if typeof tctx t1 = TyBool then
        let tyT2 = typeof tctx t2 in
        if typeof tctx t3 <: tyT2 then tyT2
        else raise (Type_error "arms of conditional have different types")
      else
        raise (Type_error "guard of conditional not a boolean")
      
    (* T-Zero *)
  | TmZero ->
      TyNat

    (* T-Succ *)
  | TmSucc t1 ->
      if typeof tctx t1 <: TyNat then TyNat
      else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | TmPred t1 ->
      if typeof tctx t1 <: TyNat then TyNat
      else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | TmIsZero t1 ->
      if typeof tctx t1 <: TyNat then TyBool
      else raise (Type_error "argument of iszero is not a number")

    (* T-Var *)
  | TmVar x ->
      (try gettbinding tctx x with
       _ -> raise (Type_error ("no binding type for variable " ^ x)))

    (* T-Abs *)
  | TmAbs (x, tyT1, t2) ->
      let tctx' = addtbinding tctx x tyT1 in
      let tyT2 = typeof tctx' t2 in
      TyArr (tyT1, tyT2)

    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = typeof tctx t1 in
      let tyT2 = typeof tctx t2 in
      (match tyT1 with
           TyArr (tyT11, tyT12) ->
             if tyT2 <: tyT11 then tyT12
             else raise (Type_error "parameter type mismatch")
         | _ -> raise (Type_error "arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let tyT1 = typeof tctx t1 in
      let tctx' = addtbinding tctx x tyT1 in
      typeof tctx' t2

(*Se ha añadido la capacidad de matchear con Fix a la función 
typeof para el apartado 2.1*)
    (* T-Fix *)
  | TmFix t1 ->
      let tyT1 = typeof tctx t1 in
      (match tyT1 with
          TyArr (tyT11, tyT12) ->
            if tyT11 <: tyT12 then tyT12
            else raise (Type_error "result of body not compatible with domain")
        | _ -> raise (Type_error "arrow type expected"))

(*Se ha añadido la capacidad de matchear con Str y StrCat a la función 
typeof para el apartado 2.3*)
    (* T-String *)
  | TmStr _ -> 
      TyStr

    (* T-^ *)
  | TmStrCat (t1, t2) ->
      if typeof tctx t1 <: TyStr then
        if typeof tctx t2 <: TyStr then TyStr
        else raise (Type_error "right argument of ^ is not a string")
      else raise (Type_error "left argument of ^ is not a string")

(*Se ha añadido la capacidad de matchear con Rcd y Proj a la función 
typeof para los apartados 2.4, 2.5 y 2.6*)
    (* T-Tuple/T-Rcd*)
  | TmRcd fdL ->
      let fnL, tmL = List.split fdL in
      TyRcd (List.combine fnL (List.map (typeof tctx) tmL))

    (* T-Proj*)
  | TmProj (t1, fn) ->
      let tyT1 = typeof tctx t1 in
      (match tyT1 with
          TyRcd tyL ->
            (try List.assoc fn tyL with
              Not_found ->
                raise (Type_error ("field " ^ fn ^ " not found")))
        | _ ->
          raise (Type_error ("can not project type " ^ string_of_ty tyT1)))

(*Se ha añadido la capacidad de matchear con Nil, Cons, IsNil, Head y
Tail a la función typeof para el apartado 2.7*)
    (* T-IsNil *)
  | TmNil ty ->
      TyList ty

    (* T-Cons *)
  | TmCons (ty, t1, t2) ->
      let tyT1, tyT2 = typeof tctx t1, typeof tctx t2 in
      (match tyT2 with
          TyList tlTy when ty <: tyT1 && ty <: tlTy -> 
            TyList ty
        | TyList tlTy -> 
            raise (Type_error ("head term of type " ^ (string_of_ty tyT1) ^
                               " and tail of type " ^ (string_of_ty tyT2) ^
                               ", in a list of " ^ (string_of_ty ty)))
        | _ ->
            raise (Type_error ("list's tail is not a term list, is " ^
                   string_of_ty tyT2)))

    (* T-IsNil *)
  | TmIsNil (ty, t1) ->
      if typeof tctx t1 <: TyList ty then TyBool
      else raise (Type_error ("argument of isnil is not a " ^
                              (string_of_ty ty) ^ "list"))

    (* T-Head *)
  | TmHead (ty, t1) ->
      if typeof tctx t1 <: TyList ty then ty
      else raise (Type_error ("argument of head is not a " ^
                              (string_of_ty ty) ^ "list"))

    (* T-Tail *)
  | TmTail (ty, t1) ->
      if typeof tctx t1 <: TyList ty then TyList ty
      else raise (Type_error ("argument of tail is not a " ^
                              (string_of_ty ty) ^ "list"))

    (* T-Unit *)
  | TmUnit ->
      TyUnit

    (* T-PrintNat *)
  | TmPrtNat t1 ->
      if typeof tctx t1 <: TyNat then TyUnit
      else raise (Type_error "argument of print_nat is not a number")

    (* T-PrintStr *)
  | TmPrtStr t1 ->
      if typeof tctx t1 <: TyStr then TyUnit
      else raise (Type_error "argument of print_string is not a string")

    (* T-ReadNat *)
  | TmRdNat t1 ->
      if typeof tctx t1 <: TyUnit then TyNat
      else raise (Type_error "argument of read_nat is not a number")

    (* T-ReadStr *)
  | TmRdStr t1 ->
      if typeof tctx t1 <: TyUnit then TyStr
      else raise (Type_error "argument of read_string is not a string")
;;


(* TERMS MANAGEMENT (EVALUATION) *)

let deformat s =
  let rec aux s = function
      [] -> s
    | (chr,str)::tl ->
      aux (String.concat str (String.split_on_char chr s)) tl
  in let chrToStrL =
    [('\n', "\\n");
     ('\r', "\\r");
     ('\b', "\\b");
     ('\t', "\\t");
     ('\\', "\\\\");
     ('"', "\\\"")]
  in
    aux s chrToStrL
;;

let print_term =
  let rec print_seqTm = function
      TmApp (TmAbs("_", TyUnit, t2), t1) ->
        open_box 1;
        print_tm t1;
        close_box ();
        print_char ';';
        print_space ();
        print_seqTm t2;
        ()
    | tm ->
        open_box 1;
        print_tm tm;
        close_box ();
        ()
  and print_tm = function
      TmIf (t1,t2,t3) ->
        print_string "if";
        print_space ();
        open_box 1;
          print_tm t1;
        close_box ();
        print_space ();
        print_string "then";
        print_space ();
        open_box 1;
          print_tm t2;
        close_box ();
        print_space ();
        print_string "else";
        print_space ();
        open_box 1;
          print_tm t3;
        close_box ();
        ()
    | TmAbs (s, tyS, t) ->
        open_box 1;
          print_string "lambda";
          print_space ();
          print_string s;
          print_char ':';
          print_ty tyS;
          print_char '.';
          print_space ();
          print_tm t;
        close_box ();
        ()
    | TmLetIn (s, t1, t2) ->
        print_string "let";
        print_space ();
        print_string s;
        print_space ();
        print_char '=';
        print_space ();
        open_box 1;
          print_tm t1;
        close_box ();
        print_space ();
        print_string "in";
        print_space ();
        open_box 1;
          print_tm t2;
        close_box ();
        ()
    | TmFix t ->
        open_box 1;
          print_string "fix";
          print_space ();
          print_tm t;
        close_box ();
        ()
    | tm ->
        print_appTm tm;
        ()

  and print_appTm = function  
      TmPred t ->
        print_string "pred";
        print_space ();
        open_box 1;
          print_projTm t;
        close_box ();
        ()
    | TmIsZero t ->
        print_string "iszero";
        print_space ();
        open_box 1;
          print_projTm t;
        close_box ();
        ()
    | TmApp (t1, t2) ->
        open_box 1;
          print_projTm t1;
          print_space ();
          print_projTm t2;
        close_box ();
        ()
    | TmStrCat (t1, t2) ->
        open_box 1;
          print_strCatTm t1;
        close_box ();
        print_space ();
        print_char '^';
        print_space ();
        open_box 1;
          print_projTm t2;
        close_box ();
        ()
    | TmCons (_, _, _) as tm ->
        let print_cons  =
          let rec aux = function
            | TmCons (_, t1, TmNil _) ->
                print_projTm t1;
                ()
            | TmCons (_, t1, t2) ->
                print_projTm t1;
                print_char ',';
                print_space ();
                aux t2
            | t ->
              print_projTm t
          in function
               TmNil ty ->
                print_string "[]";
                print_char ':';
                print_ty ty;
                ()
            | TmCons (ty, t1, TmNil _) ->
                print_char '[';
                open_box 1;
                  print_projTm t1;
                close_box ();
                print_char ']';
                print_char ':';
                print_ty ty;
                ()
            | TmCons (ty, _, _) as t ->
                print_char '[';
                open_box 1;
                  aux t;
                close_box ();
                print_char ']';
                print_char ':';
                print_ty ty;
                ()
            | t ->
                print_projTm t;
                ()
        in
          print_cons tm;
          ()
    | TmIsNil (ty, t) ->
        print_string "isnil";
        print_cut ();
        print_char '[';
        print_ty ty;
        print_char ']';
        print_space ();
        open_box 1;
          print_projTm t;
        close_box ();
        ()
    | TmHead (ty, t) ->
        print_string "head";
        print_cut ();
        print_char '[';
        print_ty ty;
        print_char ']';
        print_space ();
        open_box 1;
          print_projTm t;
        close_box ();
        ()
    | TmTail (ty, t) ->
        print_string "tail";
        print_cut ();
        print_char '[';
        print_ty ty;
        print_char ']';
        print_space ();
        open_box 1;
          print_projTm t;
        close_box ();
        ()
    | TmPrtNat t -> 
        print_string "print_nat";
        print_space ();
        open_box 1;
          print_projTm t;
        close_box ();
        ()
    | TmPrtStr t ->
        print_string "print_string";
        print_space ();
        open_box 1;
          print_projTm t;
        close_box ();
        ()
    | TmRdNat t ->
        print_string "read_nat";
        print_space ();
        open_box 1;
          print_projTm t;
        close_box ();
        ()
    | TmRdStr t ->
        print_string "read_string";
        print_space ();
        open_box 1;
          print_projTm t;
        close_box ();
        ()
    | tm ->
        print_projTm tm;
        ()

  and print_strCatTm = function
      TmStrCat (t1, t2) ->
        open_box 1;
          print_strCatTm t1;
        close_box ();
        print_space ();
        print_char '^';
        print_space ();
        open_box 1;
          print_projTm t2;
        close_box ();
        ()
    | tm ->
        open_box 1;
          print_projTm tm;
        close_box ();
        ()

  and print_projTm = function
      TmProj (t1, fn) ->
        open_box 1;
          print_projTm t1;
        close_box ();
        print_char '.';
        print_string fn;
    | TmRcd [] as tm ->
        print_atomicTm tm;
    | TmRcd fdL when isnat (fst (List.hd fdL)) ->
        let rec aux = function
            [] ->
              ()
          | h::[] ->
              print_projTm h;
              ()
          | h::t ->
              print_projTm h;
              print_char ',';
              print_space ();
              aux t;
        in
          print_char '{';
          open_box 1;
            aux (snd (List.split fdL));
          close_box ();
          print_char '}';
          ()
    | TmRcd fdL ->
        let rec aux = function
            [] ->
              ()
          | (fn,tm)::[] ->
              print_string fn;
              print_char '=';
              print_projTm tm;
              ()
          | (fn,tm)::t ->
              print_string fn;
              print_char '=';
              print_projTm tm;
              print_char ',';
              print_space ();
              aux t;
        in
          print_char '{';
          open_box 1;
            aux fdL;
          close_box ();
          print_char '}';
          ()
    | tm ->
        print_atomicTm tm;
        ()

  and print_atomicTm = function
     TmTrue ->
        print_string "true";
        ()
    | TmFalse ->
        print_string "false";
        ()
    | TmZero ->
        print_char '0';
        ()
    | TmVar s ->
        print_string s;
        ()
    | TmSucc t ->
        let rec f n t' = match t' with
            TmZero -> print_int n; ()
          | TmSucc s -> f (n+1) s
          | _ -> 
            print_string "succ";
            print_space ();
            open_box 1;
              print_projTm t;
            close_box ();
            ()
        in f 1 t
    | TmStr s ->
        print_char '\"';
        print_string (deformat s);
        print_char '\"';
        ()
    | TmNil ty ->
        print_string "[]";
        print_char ':';
        print_ty ty;
        ()
    | TmRcd [] ->
        print_string "{}";
        ()
    | TmUnit ->
        print_string "unit";
        ()
    | tm ->
        print_char '(';
        open_box 1;
          print_seqTm tm;
        close_box ();
        print_char ')';
        ()
  in fun tm -> print_seqTm tm
;;

let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

let rec free_vars tm = match tm with
    TmTrue ->
      []
  | TmFalse ->
      []
  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | TmZero ->
      []
  | TmSucc t ->
      free_vars t
  | TmPred t ->
      free_vars t
  | TmIsZero t ->
      free_vars t
  | TmVar s ->
      [s]
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
(*Se ha añadido la posibilidad de trabajar con Fix a free_vars
para el desarrollo del apartado 2.1*) 
  | TmFix t ->
      free_vars t
(*Se ha añadido la posibilidad de trabajar con Str y StrCat
 a free_vars para el desarrollo del apartado 2.3*) 
  | TmStr s ->
      []
  | TmStrCat (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
(*Se ha añadido la posibilidad de trabajar con Rcd y Proj
 a free_vars para el desarrollo de los apartados 2.4, 2.5, 2.6*) 
  | TmRcd fdL ->
      let _, tmL = List.split fdL in
      List.fold_left lunion [] (List.map free_vars tmL)
  | TmProj (t1, _) ->
      free_vars t1
(*Se ha añadido la posibilidad de trabajar con Nil, Cons, IsNil, Head
y Tail a free_vars para el desarrollo del apartado 2.7*)
  | TmNil _ ->
      []
  | TmCons (_, t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmIsNil (_, t1) ->
      free_vars t1
  | TmHead (_, t1) ->
      free_vars t1
  | TmTail (_, t1) ->
      free_vars t1
  | TmUnit ->
      []
  | TmPrtNat t1 -> 
      free_vars t1
  | TmPrtStr t1 ->
      free_vars t1
  | TmRdNat t1 ->
      free_vars t1
  | TmRdStr t1 ->
      free_vars t1
;;

let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;
    
let rec subst x s tm = match tm with
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
  | TmIf (t1, t2, t3) ->
      TmIf (subst x s t1, subst x s t2, subst x s t3)
  | TmZero ->
      TmZero
  | TmSucc t ->
      TmSucc (subst x s t)
  | TmPred t ->
      TmPred (subst x s t)
  | TmIsZero t ->
      TmIsZero (subst x s t)
  | TmVar y ->
      if y = x then s else tm
  | TmAbs (y, tyY, t) -> 
      if y = x then tm
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, tyY, subst x s t)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst x s (subst y (TmVar z) t))  
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1, t2)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmLetIn (y, subst x s t1, subst x s t2)
           else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2))
(*Se ha añadido la posibilidad de trabajar con Fix a subst
para el desarrollo del apartado 2.1*) 
  | TmFix t ->
      TmFix (subst x s t)
(*Se ha añadido la posibilidad de trabajar con Str y StrCat a subst
para el desarrollo del apartado 2.3*) 
  | TmStr st ->
      TmStr st
  | TmStrCat (t1, t2) ->
      TmStrCat (subst x s t1, subst x s t2)
(*Se ha añadido la posibilidad de trabajar con Rcd y Proj a subst
para el desarrollo de los apartados 2.4, 2.5 y 2.6*) 
  | TmRcd fdL ->
      let fnL, tmL = List.split fdL in
      TmRcd (List.combine fnL (List.map (subst x s) tmL))
  | TmProj (t1, fn) ->
      TmProj (subst x s t1, fn)
  | TmNil ty ->
      TmNil ty
  | TmCons (ty, t1, t2) ->
      TmCons (ty, subst x s t1, subst x s t2)
(*Se ha añadido la posibilidad de trabajar con Nil, Cons, IsNil, Head
y Tail a subst para el desarrollo del apartado 2.7*) 
  | TmIsNil (ty, t1) ->
      TmIsNil (ty, subst x s t1)
  | TmHead (ty, t1) ->
      TmHead (ty, subst x s t1)
  | TmTail (ty, t1) ->
      TmTail (ty, subst x s t1)
  | TmUnit ->
      TmUnit
  | TmPrtNat t1 -> 
      TmPrtNat (subst x s t1)
  | TmPrtStr t1 ->
      TmPrtStr (subst x s t1)
  | TmRdNat t1 ->
      TmRdNat (subst x s t1)
  | TmRdStr t1 ->
      TmRdStr (subst x s t1)
;;

let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;

let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmAbs _ -> true
  | t when isnumericval t -> true
(*Se ha añadido la posibilidad de trabajar con Str a isval
para el desarrollo del apartado 2.3*) 
  | TmStr _ -> true
(*Se ha añadido la posibilidad de trabajar con Rcd a isval
para el desarrollo de los apartados 2.4, 2.5 y 2.6*) 
  | TmRcd fdL when let _, tmL = List.split fdL in
                   List.for_all isval tmL -> true
(*Se ha añadido la posibilidad de trabajar con Nil y Cons a isval
para el desarrollo del apartado 2.7*) 
  | TmNil _ -> true
  | TmCons (_, t1, t2) when isval t1 && isval t2 -> true
  | TmUnit -> true
  | _ -> false
;;

exception NoRuleApplies
;;

let rec eval1 vctx tm = match tm with
    (* E-IfTrue *)
    TmIf (TmTrue, t2, _) ->
      t2

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1' = eval1 vctx t1 in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | TmSucc t1 ->
      let t1' = eval1 vctx t1 in
      TmSucc t1'

    (* E-PredZero *)
  | TmPred TmZero ->
      TmZero

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1

    (* E-Pred *)
  | TmPred t1 ->
      let t1' = eval1 vctx t1 in
      TmPred t1'

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      TmTrue

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse

    (* E-Iszero *)
  | TmIsZero t1 ->
      let t1' = eval1 vctx t1 in
      TmIsZero t1'

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst x v2 t12

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 vctx t2 in
      TmApp (v1, t2')

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 vctx t1 in
      TmApp (t1', t2)

    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      subst x v1 t2

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      let t1' = eval1 vctx t1 in
      TmLetIn (x, t1', t2) 

(*Se ha añadido la posibilidad de trabajar con Fix a eval1
para el desarrollo del apartado 2.1. Se han implementado dos
posibilidades, una para trabajar con abstracciones y otra para
aplicar el Fix*)
    (* E-FixBeta *)
  | TmFix (TmAbs (x, _, t12)) ->
      subst x tm t12 

    (* E-Fix *)
  | TmFix t1 ->
      let t1' = eval1 vctx t1 in
      TmFix t1'

(*Se ha añadido la posibilidad de trabajar con StrCat a eval1
para el desarrollo del apartado 2.3. Se han implementado tres
posibilidades, una para ejecutar la concatenación, otra para
la posibilidad de que el primer término sea ya string, y otra
para la posibilidad de que lo sea el segundo*)
    (* E-StrCat *)
  | TmStrCat (t1, t2) when isval t1 && isval t2 ->
      (match t1, t2 with
          (TmStr s1, TmStr s2) ->
              TmStr (s1 ^ s2)
        | _ -> raise NoRuleApplies)

    (* E-StrCat2 *)
  | TmStrCat (t1, t2) when isval t1 ->
      let t2' = eval1 vctx t2 in
      TmStrCat (t1, t2')

    (* E-StrCat1 *)
  | TmStrCat (t1, t2) ->
      let t1' = eval1 vctx t1 in
      TmStrCat (t1', t2)

(*Se ha añadido la posibilidad de trabajar con Rcd y Proj a eval1
para el desarrollo de los apartados 2.4, 2.5 y 2.6. En el caso de 
Proj hay dos posibilidades, una evalua, y la otra ejecuta la proyección*)
    (* E-Tuple/E-Rcd *)
  | TmRcd fdL when not (isval tm) ->
      let rec rcd_deep_eval tm = match tm with
          TmRcd fdL ->
            let fnL, tmL = List.split fdL in
            TmRcd (List.combine fnL (List.map (rcd_deep_eval) tmL))
        | tm when isval tm -> tm
        | tm -> eval1 vctx tm
      in
        rcd_deep_eval tm

    (* E-ProjRCD\E-ProjTuple *)
  | TmProj (t1, fn) when isval t1 ->
      (match t1 with
          TmRcd fdL -> List.assoc fn fdL
        | _ -> raise NoRuleApplies)

    (* E-Proj *)
  | TmProj (t1, fn) ->
      let t1' = eval1 vctx t1 in
      TmProj (t1', fn)

(*Se ha añadido la posibilidad de trabajar con Cons, Nil, IsNil, Head y
Tail a eval1 para el desarrollo del apartado 2.7. En el caso de 
Cons hay dos posibilidades, una evalua el primer término antes del segundo,
y la otra a la inversa. En IsNil tenemos también tres posibilidades; la
que se diferencian en función de si la lista que recibe es vacía, llena
o el caso restante. En Head y Tail tenemos dos posibilidades; en una si el
término es una lista devolverá la cabeza o la cola (correspondientemente)
y en la otra se evalua el término*)

    (* E-Cons2 *)
  | TmCons (ty, t1, t2) when isval t1 ->
      let t2' = eval1 vctx t2 in
      TmCons (ty, t1, t2')

    (* E-Cons1 *)
  | TmCons (ty, t1, t2) ->
      let t1' = eval1 vctx t1 in
      TmCons (ty, t1', t2)

    (* E-IsNilNil *)
  | TmIsNil (_, TmNil _) ->
      TmTrue

    (* E-IsNilCons *)
  | TmIsNil (_, TmCons (_, _, _)) ->
      TmFalse

    (* E-IsNil *)
  | TmIsNil (ty, t1) ->
      let t1' = eval1 vctx t1 in
      TmIsNil (ty, t1')

    (* E-HeadCons *)
  | TmHead (_, TmCons (_, t1, _)) ->
      t1

    (* E-Head *)
  | TmHead (ty, t1) ->
      let t1' = eval1 vctx t1 in
      TmHead (ty, t1')

    (* E-TailCons *)
  | TmTail (ty, TmCons (_, _, t1)) ->
      t1

    (* E-Tail *)
  | TmTail (ty, t1) ->
      let t1' = eval1 vctx t1 in
      TmTail (ty, t1')

    (* E-PrintNat1 *)
  | TmPrtNat t1 when isnumericval t1 ->
      let rec f = function
          TmZero -> 0
        | TmSucc tm -> 1+(f tm)
        | _ -> -1
      in
        print_int (f t1);
        print_flush ();
        TmUnit

    (* E-PrintNat *)
  | TmPrtNat t1 ->
      let t1' = eval1 vctx t1 in
      TmPrtNat t1'

    (* E-PrintString1 *)
  | TmPrtStr (TmStr s) ->
      print_string s;
      print_flush ();
      TmUnit

    (* E-PrintString *)
  | TmPrtStr t1 ->
      let t1' = eval1 vctx t1 in
      TmPrtStr t1'

    (* E-ReadNat1 *)
  | TmRdNat t1 when isval t1 ->
      let rec f = function
          0 -> TmZero
        | n -> TmSucc (f (n-1))
      in f (read_int ())

    (* E-ReadNat *)
  | TmRdNat t1 ->
      let t1' = eval1 vctx t1 in
      TmRdNat t1'

    (* E-ReadString1 *)
  | TmRdStr t1 when isval t1 ->
      TmStr (read_line ())

    (* E-ReadString *)
  | TmRdStr t1 ->
      let t1' = eval1 vctx t1 in
      TmRdStr t1'

  | TmVar s ->
      getvbinding vctx s

  | _ ->
      raise NoRuleApplies
;;

let apply_ctx ctx tm =
  List.fold_left (fun t x -> subst x (getvbinding ctx x) t ) tm (free_vars tm)
;;

let rec eval vctx tm =
  try
    let tm' = eval1 vctx tm in
    eval vctx tm'
  with
    NoRuleApplies -> apply_ctx vctx tm
;;

let execute (vctx, tctx) = function
    Ignore ->
      (vctx, tctx)

  | Eval tm ->
      let _ = typeof tctx tm in
      let _ = eval vctx tm in 
      (vctx, tctx)

  | Bind (s, tm) ->
      let tyTm = typeof tctx tm in
      let tm' = eval vctx tm in
      (addvbinding vctx s tm', addtbinding tctx s tyTm)
;;

(*Para realizar el apartado 1.2, se realizó las funciones; 'print_term', para mostrar
el valor de un término pero eliminado todos los paréntesis posibles mediante funciones 
recursivas que imitan el recorrido del arbol formado por la gramatica; 'print_ty',
para mostrar el tipo de un término de la misma forma que la anterior función; y 
pretty_printer', que llama a estas dos funciones para mostrar el resultado de evaluar
el término insertado. Estas funciones usan el módulo 'Format' para imprimir en la
salida estándar con una correcta indentización mediante una estructura basada en
bloques.*)

let pretty_printer s tyTm tm' =
  open_box 1;
    print_string s;
    print_space ();
    print_char ':';
    print_space ();
    print_ty tyTm;
    print_space ();
    print_char '=';
    print_space ();
    open_box 1;
      print_term tm';
    close_box ();
  close_box ();
  force_newline ();
  print_flush ();
;;

let executeAndPrint (vctx, tctx) = function
    Ignore ->
      (vctx, tctx)

  | Eval tm ->
      let tyTm = typeof tctx tm in
      let tm' = eval vctx tm in 
      pretty_printer "-" tyTm tm';
      (vctx, tctx)

  | Bind (s, tm) ->
      let tyTm = typeof tctx tm in
      let tm' = eval vctx tm in
      pretty_printer s tyTm tm';
      (addvbinding vctx s tm', addtbinding tctx s tyTm)
;;
