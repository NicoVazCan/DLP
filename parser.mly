
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
%token LBRACE
%token RBRACE
%token LBRACK
%token RBRACK
%token NIL
%token CONS
%token ISNIL
%token HEAD
%token TAIL
%token LIST
%token UNIT
%token UNIT_TY
%token DOT
%token COMMA
%token DOT_COMMA
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
    STRINGV EQ seqTerm EOF
      { Bind ($1, $3) }
  | seqTerm EOF
      { Eval $1 }

seqTerm:
    term DOT_COMMA seqTerm
      { TmApp (TmAbs("_", TyUnit, $3), $1) }
  | term
      { $1 }

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

projTerm :
    atomicTerm
      { $1 }
  | brackTerm
      { $1 }
  | projTerm DOT INTV
      {TmProj ($1, string_of_int $3)}
  | projTerm DOT STRINGV
      {TmProj ($1, $3)}

brackTerm:
    LBRACE posTerms RBRACE
      { TmRcd (List.combine (List.init (List.length $2) string_of_int) $2) }
  | LBRACE fieldTerms RBRACE
      { TmRcd $2}
  | LBRACK RBRACK COLON ty
      { TmNil $4 }
  | LBRACK posTerms RBRACK COLON ty
      { List.fold_left (fun t1 t2 -> TmCons ($5, t2, t1)) (TmNil $5) (List.rev $2) }

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

appTerm :
    projTerm
      { $1 }
  | SUCC projTerm
      { TmSucc $2 }
  | PRED projTerm
      { TmPred $2 }
  | ISZERO projTerm
      { TmIsZero $2 }
  | appTerm projTerm
      { TmApp ($1, $2) }

  | srtCatTerm STRCAT projTerm
      { TmStrCat ($1, $3) }
  | CONS listTy projTerm projTerm
      { TmCons ($2, $3, $4) }
  | ISNIL listTy projTerm
      { TmIsNil ($2, $3)}
  | HEAD listTy projTerm
      { TmHead ($2, $3)}
  | TAIL listTy projTerm
      { TmTail ($2, $3)}

srtCatTerm:
    srtCatTerm STRCAT projTerm
      { TmStrCat ($1, $3) }
  | projTerm
      { $1 }

listTy:
    LBRACK ty RBRACK
      { $2 }

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
  | NIL listTy
      { TmNil $2 }
  | LBRACE RBRACE
      { TmRcd [] }
  | UNIT
      { TmUnit }


ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }
  | LBRACE RBRACE
      { TyRcd [] }
  | LBRACE posTys RBRACE
      { TyRcd (List.combine (List.init (List.length $2) string_of_int) $2) }
  | LBRACE fieldTys RBRACE
      { TyRcd $2}
  | LIST ty
      { TyList $2}

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
  | UNIT_TY
      { TyUnit }

