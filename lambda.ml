
(* TYPE DEFINITIONS *)

type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyStr
  | TyRcd of (string * ty) list
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
;;

type vcontext =
  (string * term) list
;;

type command = 
    Eval of term
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
  | TyRcd tyL when isnat (fst (List.hd tyL)) ->
      let sFdL = List.map (fun (_, ty) -> string_of_ty ty) tyL in
      "{" ^ (String.concat ", " sFdL) ^ "}"
  | TyRcd tyL ->
      let sFdL = List.map (fun (fn, ty) -> fn ^ ":" ^ string_of_ty ty) tyL in
      "{" ^ (String.concat ", " sFdL) ^ "}"
      
;;

exception Type_error of string
;;

let rec typeof tctx tm = match tm with
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
        if typeof tctx t3 = tyT2 then tyT2
        else raise (Type_error "arms of conditional have different types")
      else
        raise (Type_error "guard of conditional not a boolean")
      
    (* T-Zero *)
  | TmZero ->
      TyNat

    (* T-Succ *)
  | TmSucc t1 ->
      if typeof tctx t1 = TyNat then TyNat
      else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | TmPred t1 ->
      if typeof tctx t1 = TyNat then TyNat
      else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | TmIsZero t1 ->
      if typeof tctx t1 = TyNat then TyBool
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
             if tyT2 = tyT11 then tyT12
             else raise (Type_error "parameter type mismatch")
         | _ -> raise (Type_error "arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let tyT1 = typeof tctx t1 in
      let tctx' = addtbinding tctx x tyT1 in
      typeof tctx' t2

    (* T-Fix *)
  | TmFix t1 ->
      let tyT1 = typeof tctx t1 in
      (match tyT1 with
          TyArr (tyT11, tyT12) ->
            if tyT11 = tyT12 then tyT12
            else raise (Type_error "result of body not compatible with domain")
        | _ -> raise (Type_error "arrow type expected"))

    (* T-String *)
  | TmStr _ -> 
      TyStr

    (* T-^ *)
  | TmStrCat (t1, t2) ->
      if typeof tctx t1 = TyStr then
        if typeof tctx t2 = TyStr then TyStr
        else raise (Type_error "right argument of ^ is not a string")
      else raise (Type_error "left argument of ^ is not a string")

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
                raise (Type_error ("field " ^ fn ^ "not found")))
        | _ ->
          raise (Type_error ("can not project type" ^ string_of_ty tyT1))
      )

;;


(* TERMS MANAGEMENT (EVALUATION) *)

let rec string_of_term = function
    TmTrue ->
      "true"
  | TmFalse ->
      "false"
  | TmIf (t1,t2,t3) ->
      "if " ^ "(" ^ string_of_term t1 ^ ")" ^
      " then " ^ "(" ^ string_of_term t2 ^ ")" ^
      " else " ^ "(" ^ string_of_term t3 ^ ")"
  | TmZero ->
      "0"
  | TmSucc t ->
     let rec f n t' = match t' with
          TmZero -> string_of_int n
        | TmSucc s -> f (n+1) s
        | _ -> "succ " ^ "(" ^ string_of_term t ^ ")"
      in f 1 t
  | TmPred t ->
      "pred " ^ "(" ^ string_of_term t ^ ")"
  | TmIsZero t ->
      "iszero " ^ "(" ^ string_of_term t ^ ")"
  | TmVar s ->
      s
  | TmAbs (s, tyS, t) ->
      "(lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ string_of_term t ^ ")"
  | TmApp (t1, t2) ->
      "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
  | TmLetIn (s, t1, t2) ->
      "let " ^ s ^ " = " ^ string_of_term t1 ^ " in " ^ string_of_term t2
  | TmFix t ->
      "(fix " ^ string_of_term t ^ ")"
  | TmStr s ->
      "\"" ^ s ^ "\""
  | TmStrCat (t1, t2) ->
      "(" ^ string_of_term t1 ^ ") ^ (" ^ string_of_term t2 ^ ")"
  | TmRcd [] ->
      "{}"
  | TmRcd fdL when isnat (fst (List.hd fdL)) ->
      let sFdL = List.map (fun (_, tm) -> string_of_term tm) fdL in
      "{" ^ (String.concat ", " sFdL) ^ "}"
  | TmRcd fdL ->
      let sFdL = List.map (fun (fn, tm) -> fn ^ "=" ^ string_of_term tm) fdL in
      "{" ^ (String.concat ", " sFdL) ^ "}"
  | TmProj (t1, fn) ->
      string_of_term t1 ^ "." ^ fn
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
  | TmFix t ->
      free_vars t
  | TmStr s ->
      []
  | TmStrCat (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmRcd fdL ->
      let _, tmL = List.split fdL in
      List.fold_left lunion [] (List.map free_vars tmL)
  | TmProj (t1, _) ->
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
  | TmFix t ->
      TmFix (subst x s t)
  | TmStr st ->
      TmStr st
  | TmStrCat (t1, t2) ->
      TmStrCat (subst x s t1, subst x s t2)
  | TmRcd fdL ->
      let fnL, tmL = List.split fdL in
      TmRcd (List.combine fnL (List.map (subst x s) tmL))
  | TmProj (t1, fn) ->
      TmProj (subst x s t1, fn)
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
  | TmStr _ -> true
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

    (* E-FixBeta *)
  | TmFix (TmAbs (x, _, t12)) ->
      subst x tm t12 

    (* E-Fix *)
  | TmFix t1 ->
      let t1' = eval1 vctx t1 in
      TmFix t1'

    (* E-^: base case *)
  | TmStrCat (TmStr s1, TmStr s2) ->
      TmStr (s1 ^ s2)

    (* E-^: evaluate right argument before concat *)
  | TmStrCat (TmStr s1, t2) -> (match eval1 vctx t2 with
        TmStr s2 -> TmStr (s1 ^ s2)
      | _ -> raise NoRuleApplies)

    (* E-^: evaluate left argument before concat *)
  | TmStrCat (t1, TmStr s2) -> (match eval1 vctx t1 with
        TmStr s1 -> TmStr (s1 ^ s2)
      | _ -> raise NoRuleApplies)

    (* E-Tuple/E-Rcd *)
  | TmRcd fdL ->
      let fnL, tmL = List.split fdL in
      TmRcd (List.combine fnL (List.map (eval1 vctx) tmL))

    (* E-Proj *)
  | TmProj (t1, fn) ->
      let t1' = eval1 vctx t1 in
      TmProj (t1', fn)

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
    Eval tm ->
      let tyTm = typeof tctx tm in
      let tm' = eval vctx tm in 
      print_endline ("- : " ^ string_of_ty tyTm
                     ^ " = " ^ string_of_term tm');
      (vctx, tctx)

  | Bind (s, tm) ->
      let tyTm = typeof tctx tm in
      let tm' = eval vctx tm in
      print_endline (s ^ " : " ^ string_of_ty tyTm
                     ^ " = " ^ string_of_term tm');
      (addvbinding vctx s tm', addtbinding tctx s tyTm)
;;

