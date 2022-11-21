
type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyStr
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
;;

type vcontext =
  (string * term) list
;;

type command = 
    Eval of term
  | Bind of string * term
;;

val emptytctx : tcontext;;
val addtbinding : tcontext -> string -> ty -> tcontext;;
val gettbinding : tcontext -> string -> ty;;

val emptyvctx : vcontext;;
val addvbinding : vcontext -> string -> term -> vcontext;;
val getvbinding : vcontext -> string -> term;;

val string_of_ty : ty -> string;;
exception Type_error of string;;
val typeof : tcontext -> term -> ty;;

val string_of_term : term -> string;;
exception NoRuleApplies;;
val eval : vcontext -> term -> term;;

val execute: vcontext * tcontext -> command -> vcontext * tcontext;;

