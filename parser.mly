
%{
  open Lambda;;
%}

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LET
%token LETREC
%token IN
%token BOOL
%token NAT
%token STRING
%token STRCAT

%token LPAREN
%token RPAREN
%token LBRACK
%token RBRACK
%token DOT
%token COMMA
%token EQ
%token COLON
%token ARROW
%token EOF

%token <int> INTV
%token <string> STRINGV
%token <string> STRINGL

%start s
%type <Lambda.command> s

%%

s :
    STRINGV EQ term EOF
      { Bind ($1, $3) }
  | term EOF
      { Eval $1 }

term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | LAMBDA STRINGV COLON ty DOT term
      { TmAbs ($2, $4, $6) }
  | LET STRINGV EQ term IN term
      { TmLetIn ($2, $4, $6) }
  | LETREC STRINGV COLON ty EQ term IN term
      { TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), $8) }

appTerm :
  | projTerm
      { $1 }
  | SUCC atomicTerm
      { TmSucc $2 }
  | PRED atomicTerm
      { TmPred $2 }
  | ISZERO atomicTerm
      { TmIsZero $2 }
  | projTerm STRCAT atomicTerm
      { TmStrCat ($1, $3) }
  | LBRACK RBRACK
      { TmRcd [] }
  | LBRACK posTerms RBRACK
      { TmRcd (List.combine (List.init (List.length $2) string_of_int) $2) }
  | LBRACK fieldTerms RBRACK
      { TmRcd $2}
  | projTerm atomicTerm
      { TmApp ($1, $2) }

projTerm :
    atomicTerm
      { $1 }
  | appTerm DOT INTV
      {TmProj ($1, string_of_int $3)}
  | appTerm DOT STRINGV
      {TmProj ($1, $3)}

posTerms:
    projTerm COMMA posTerms
      { $1::$3 }
  | projTerm COMMA 
      { $1::[] }
  | projTerm 
      { $1::[] }

fieldTerms:
    STRINGV EQ projTerm COMMA fieldTerms
      { ($1, $3)::$5 }
  | STRINGV EQ projTerm COMMA
      { ($1, $3)::[] }
  | STRINGV EQ projTerm
      { ($1, $3)::[] }

atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | STRINGV
      { TmVar $1 }
  | INTV
      { let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f $1 }
  | STRINGL
      { TmStr $1 }

ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }

atomicTy :
    LPAREN ty RPAREN  
      { $2 } 
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | STRING
      { TyStr }

