
type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
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
;;

type vcontext =
  (string * ty) list
;;

type command = 
    Eval of term
  | Bind of string * term
;;

val emptytctx : context;;
val addtbinding : context -> string -> ty -> context;;
val gettbinding : context -> string -> ty;;

val emptyvctx : context;;
val addvbinding : context -> string -> ty -> context;;
val getvbinding : context -> string -> ty;;

val string_of_ty : ty -> string;;
exception Type_error of string;;
val typeof : context -> term -> ty;;

val string_of_term : term -> string;;
exception NoRuleApplies;;
val eval : term -> term;;

