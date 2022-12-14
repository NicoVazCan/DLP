
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
/*The token "LETREC" has been added for the purpose of identifying
the recursive expressions of section 2.1*/
%token LETREC
%token IN
%token BOOL
%token NAT
%token STRING
%token STRCAT
%token LPAREN
%token RPAREN
/*Added tokens "LBRACE", "RBRACE", "LBRACK",
"RBRACK", "NIL", "CONS", "ISNIL", "HEAD", "TAIL", "LIST" HAVE BEEN ADDED TO THE LEXER
in the lexer in order to identify the characters of the
lists of section 2.7*/
%token LBRACE
%token RBRACE
/*The tokens "LBRACK", "RBRACK" and "COMMA" have been added to the lexer to identify the characters of the 
in the lexer in order to identify the characters of pairs, tuples and records in
pairs, tuples and records in sections 2.4, 2.5 and 2.6.*/
%token LBRACK
%token RBRACK
%token NIL
%token CONS
%token ISNIL
%token HEAD
%token TAIL
%token LIST
/*The tokens "UNIT", "UNIT_TY" and "DOT_COMMA" have been added in the lexer in order to identify the characters of the
in the lexer in order to identify the characters in the lists of section 2.9.1.2.
lists of section 2.9*/
%token UNIT
%token UNIT_TY
%token P_NAT
%token P_STRING
%token R_NAT
%token R_STRING
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

/*An expression has been created to form sequences of Terms
(section 2.9)*/
s :
      { Ignore }
  | STRINGV EQ seqTerm EOF
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
/*This syntax has been added for the operation of section 2.1.
section 2.1, to adapt Fix to the typed language.*/
  | LETREC STRINGV COLON ty EQ term IN term
      { TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), $8) }

/*projTerm, posTerms and fieldTerms have been created. The first one to
projection, the second to capture the elements of a tuple, and the third to capture the tag and terms of a
of a tuple, and the third one to capture the tag and the terms of a record.*/
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
/*For convenience the lists can be indicated as in most languages*/
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
/*The ability to work with proj, LBRACK and RBRACK has been added to appTerm for the development of sections 2.4, 2.5 and 2.6.
to appTerm for the development of sections 2.4, 2.5 and 2.6.*/
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

/*Added the ability to work with "LBRACE", "RBRACE", "LBRACK",
"RBRACK", "NIL", "CONS", "ISNIL", "HEAD", "TAIL" and "LIST" to appTerm for the development of section 2.7.
to appTerm for the development of section 2.7*/
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
/*The ability to work with P_NAT, P_STRING, R_NAT, and R_STRING has been added to appTerm for the development of section 2.10.
R_STRING to appTerm for the development of section 2.10.*/
  | P_NAT projTerm
      { TmPrtNat $2 }
  | P_STRING projTerm
      { TmPrtStr $2 }
  | R_NAT projTerm
      { TmRdNat $2 }
  | R_STRING projTerm
      { TmRdStr $2 }

srtCatTerm:
    srtCatTerm STRCAT projTerm
      { TmStrCat ($1, $3) }
  | projTerm
      { $1 }

/*Term to indicate the type of the list delivered to the appterm
(section 2.7)*/
listTy:
    LBRACK ty RBRACK
      { $2 }

atomicTerm :
    LPAREN seqTerm RPAREN
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
/*For convenience the lists can be indicated as in most languages*/
  | LBRACK RBRACK COLON ty
      { TmNil $4 }
  | LBRACE RBRACE
      { TmRcd [] }
/*The term Unit has been added for the development of section 2.9.*/
  | UNIT
      { TmUnit }


ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }
  
  | LIST ty
      { TyList $2}

atomicTy :
    LPAREN ty RPAREN  
      { $2 } 
  | BOOL
      { TyBool }
  | NAT
      { TyNat }

  | STRING
      { TyStr }
  | LBRACE RBRACE
      { TyRcd [] }
  | brackTy
      { $1 }
/*Se ha añadido el tipo Unit para el desarrollo del apartado 2.9*/
  | UNIT_TY
      { TyUnit }

brackTy :
    LBRACE posTys RBRACE
      { TyRcd (List.combine (List.init (List.length $2) string_of_int) $2) }
  | LBRACE fieldTys RBRACE
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
