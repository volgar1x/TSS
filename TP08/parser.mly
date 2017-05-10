%{
  open Expression ;;
%}

%token Leol
%token Loparen
%token Lcparen
%token Lobrack
%token Lcbrack
%token Loangle
%token Lcangle
%token Lbar
%token Llambda
%token Lcomma
%token Ldot
%token Lcolon
%token Lbang
%token Larrow
%token Lfatarrow
%token Lletrec
%token Leq
%token Lassign
%token <string> Lident
%token <string> Linteger
%token Ltrue
%token Lfalse
%token Lif
%token Lthen
%token Lelse
%token Llet
%token Lin
%token Lend
%token Lcolon
%token Lunit
%token Las
%token Lcase
%token Lof
%token Ltyperef
%token Lunderscore

%start line
%type <Expression.expression> line
%type <Expression.type_> type
%type <(string * Expression.expression) list> recordlist
/*%type <(string * Expression.type) list> typerecordlist*/
/*%type <(string * string * Expression.expression) list> variantlist*/
/*%type <(string * Expression.type) list> typevariantlist*/

%%

line :
     | expr Lend                           {$1}
;

expr :
     | Lident Lassign expr                                                 {Assign ($1, $3)}
     | Lletrec Lident Lcolon type Leq expr Lin expr                        {DefineRecFunc ($2, $4, $6, $8)}
     | Llet Lident Leq expr Lin expr                                       {Local ($2, $4, $6)}
     | Llet Lident Leq expr                                                {Global ($2, $4)}
     | Llambda Lident Lcolon type Ldot expr                                {Function ($2, $4, $6)}
     | Lif expr Lthen expr Lelse expr                                      {Cond ($2, $4, $6)}
     | Loangle Lident Leq expr Lcangle Las Loangle typevariantlist Lcangle {Variant ($2, $4, Variant ($8))}
     | Lcase expr Lof variantlist                                          {Case  ($2, $4)}
     | expr1                                                               {$1}
     | expr Lcolon expr                                                    {Each ($1, $3)}
;

expr1 :
      | expr1 expr2 { Application ($1, $2) }
      | expr2       { $1 }
;

expr2 :
      | Lbang Lident               { Access ($2) }
      | Lident Ldot Lident         { Proj (Variable $1, $3) }
      | Lident                     { Variable ($1) }
      | Ltrue                      { Boolean (true) }
      | Lfalse                     { Boolean (false) }
      | Linteger                   { Natural (int_of_string $1) }
      | Lunit                      { Unit }
      | Lobrack recordlist Lcbrack { Record ($2) }
      | Loparen expr Lcparen       { $2 }
;

type :
     | type1 Larrow type           { Apply ($1, $3) }
     | type1                       { $1 }
;

type1 :
      | Lident                          { type_of_string $1 }
      | Loparen type Lcparen            { $2 }
      | Lobrack typerecordlist Lcbrack  { Record $2 }
      | Loangle typevariantlist Lcangle { Variant ($2) }
      | Ltyperef type                   { Ref ($2) }
;

recordlist :
           | Lident Leq expr recordlist2 { Assoc.put $1 $3 $4 }
           |                             { [] }
;

recordlist2 :
            | Lcomma recordlist { $2 }
            |                   { [] }
;

typerecordlist :
           | Lident Lcolon type typerecordlist2 { Assoc.put $1 $3 $4 }
           |                                    { [] }
;

typerecordlist2 :
            | Lcomma typerecordlist { $2 }
            |                       { [] }
;

variant :
        | Loangle Lident Leq Lident Lcangle Lfatarrow expr { VariantCase ($2, $4, $7) }
        | Lunderscore Lfatarrow expr                       { VariantFallthrough ($3) }
;

variantlist :
            | { [] }
            | Lbar variant variantlist { $2 :: $3 }
;

typevariantlist :
                | Lident Lcolon type typevariantlist2 { Assoc.put $1 $3 $4 }
                |                                     { [] }
;

typevariantlist2 :
                 | Lbar typevariantlist { $2 }
                 |                      { [] }
;

%%
