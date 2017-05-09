%{
  open Expression ;;
%}

%token Leol
%token Loparen
%token Lcparen
%token Lobrack
%token Lcbrack
%token Llambda
%token Lcomma
%token Ldot
%token Lcolon
%token Larrow
%token Lglobal
%token Leq
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

%start line
%type <Expression.expression> line
%type <Expression.type_> type
%type <(string * Expression.expression) list> recordlist

%%

line :
     | expr Lend                           {$1}
;

expr :
     | Llet Lident Leq expr Lin expr           {Local ($2, $4, $6)}
     | Llet Lident Leq expr                    {Global ($2, $4)}
     | Llambda Lident Lcolon type Ldot expr    {Function ($2, $4, $6)}
     | Lif expr Lthen expr Lelse expr          {Cond ($2, $4, $6)}
     | expr1                                   {$1}
     | expr Lcolon expr                        {Each ($1, $3)}
;

expr1 :
      | expr1 expr2 { Application ($1, $2) }
      | expr2       { $1 }
;

expr2 :
      | Lident                     { Variable ($1) }
      | Ltrue                      { Boolean (true) }
      | Lfalse                     { Boolean (false) }
      | Linteger                   { Natural (int_of_string $1) }
      | Lunit                      { Unit }
      | Lobrack recordlist Lcbrack { Record ($2) }
      | Loparen expr Lcparen       { $2 }
;

type :
     | type1 Larrow type     { Apply ($1, $3) }
     | type1                  { $1 }
;

type1 :
      | Lident               { type_of_string $1 }
      | Loparen type Lcparen { $2 }
;

recordlist :
           | Lident Leq expr recordlist2 { ($1, $3) :: $4 }
           |                             { [] }
;

recordlist2 :
            | Lcomma recordlist { $2 }
            |                   { [] }
;

%%
