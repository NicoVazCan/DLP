
type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
(*Se ha añadido el tipo String para el desarrollo del apartado 2.3*)
  | TyStr
(*Se ha añadido el tipo Registro para el desarrollo de los apartados 2.4, 2.5 y 2.6*)
  | TyRcd of (string * ty) list
(*Se ha añadido el tipo Lista para el desarrollo del apartado 2.7*)
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
(*Se ha añadido el término Fix para el desarrollo del apartado 2.1*)
  | TmFix of term
(*Se ha añadido el término Str para el desarrollo del apartado 2.3*)
  | TmStr of string
(*Se ha añadido el término StrCat para el desarrollo del apartado 2.3*)
  | TmStrCat of term * term
(*Se ha añadido los términos TmRcd y TmProj para el desarrollo de los apartados 2.4, 2.5 y 2.6*)
  | TmRcd of (string * term) list
  | TmProj of term * string
(*Se ha añadido los términos TmNil, TmCons, TmIsNil, TmHead, TmTail para el desarrollo del apartado 2.7*)
  | TmNil of ty
  | TmCons of ty * term * term
  | TmIsNil of ty * term
  | TmHead of ty * term
  | TmTail of ty * term
  | TmUnit
(*Se ha añadido los términos TmPrtNat, TmPrtStr, TmPrtStr, TmRdNat, TmRdStr para
el desarrollo del apartado 2.10*)
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

