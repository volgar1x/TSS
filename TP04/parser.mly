%{
  open Expression ;;
%}

%token Leol
%token Loparen
%token Lcparen
%token Llambda
%token Ldot
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

%start line
%type <Expression.expression> line

%%

line :
     | expr Lend                           {$1}
;

expr :
     | Llet Lident Leq expr      {Global ($2, $4)}
     | Llambda Lident Ldot expr  {Function ($2, $4)}
     | expr1                     {$1}
;

expr1 :
      | expr1 expr2 { Application ($1, $2) }
      | expr2       { $1 }
;

expr2 :
      | Lident               { Variable ($1) }
      | Loparen expr Lcparen { $2 }
;

%%
