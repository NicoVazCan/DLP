
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

val emptytctx : tcontext;;
val addtbinding : tcontext -> string -> ty -> tcontext;;
val gettbinding : tcontext -> string -> ty;;

val emptyvctx : vcontext;;
val addvbinding : vcontext -> string -> term -> vcontext;;
val getvbinding : vcontext -> string -> term;;

exception Type_error of string;;

exception NoRuleApplies;;

val execute: vcontext * tcontext -> command -> vcontext * tcontext;;
val executeAndPrint: vcontext * tcontext -> command -> vcontext * tcontext;;

