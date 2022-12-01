
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
    projTerm
      { $1 }
  | SUCC projTerm
      { TmSucc $2 }
  | PRED projTerm
      { TmPred $2 }
  | ISZERO projTerm
      { TmIsZero $2 }
  | srtCatTerm STRCAT projTerm
      { TmStrCat ($1, $3) }
  | appTerm projTerm
      { TmApp ($1, $2) }

srtCatTerm:
    srtCatTerm STRCAT projTerm
      { TmStrCat ($1, $3) }
  | projTerm
      { $1 }

projTerm :
    atomicTerm
      { $1 }
  | projTerm DOT INTV
      {TmProj ($1, string_of_int $3)}
  | projTerm DOT STRINGV
      {TmProj ($1, $3)}

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
  | LBRACK RBRACK
      { TmRcd [] }
  | LBRACK posTerms RBRACK
      { TmRcd (List.combine (List.init (List.length $2) string_of_int) $2) }
  | LBRACK fieldTerms RBRACK
      { TmRcd $2}

posTerms :
    projTerm COMMA posTerms
      { $1::$3 }
  | projTerm COMMA 
      { $1::[] }
  | projTerm 
      { $1::[] }

fieldTerms :
    STRINGV EQ projTerm COMMA fieldTerms
      { ($1, $3)::$5 }
  | STRINGV EQ projTerm COMMA
      { ($1, $3)::[] }
  | STRINGV EQ projTerm
      { ($1, $3)::[] }

ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }
  | LBRACK RBRACK
      { TyRcd [] }
  | LBRACK posTys RBRACK
      { TyRcd (List.combine (List.init (List.length $2) string_of_int) $2) }
  | LBRACK fieldTys RBRACK
      { TyRcd $2}

posTys :
    ty COMMA posTys
      { $1::$3 }
  | ty COMMA 
      { $1::[] }
  | ty 
      { $1::[] }

fieldTys:
    STRINGV COLON ty COMMA fieldTys
      { ($1, $3)::$5 }
  | STRINGV COLON ty COMMA
      { ($1, $3)::[] }
  | STRINGV COLON ty
      { ($1, $3)::[] }

atomicTy :
    LPAREN ty RPAREN  
      { $2 } 
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | STRING
      { TyStr }

