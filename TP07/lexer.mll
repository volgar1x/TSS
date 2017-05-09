{
  open Parser ;;

  let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = 0;
    }
  ;;

  let incr_colnum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_bol = pos.Lexing.pos_bol + 1;
    }
  ;;

}

rule lexer = parse

  | [' ' '\t']                                          {lexer lexbuf}
  | '#' [^'\n']*                                        {lexer lexbuf}
  | '\n'                                                {incr_linenum lexbuf; lexer lexbuf}
  | eof                                                 {raise End_of_file}

  | "("                                                 {Loparen}
  | ")"                                                 {Lcparen}
  | "lambda"                                            {Llambda}
  | "."                                                 {Ldot}
  | ":"                                                 {Lcolon}
  | "->"                                                {Larrow}
  | "global"                                            {Lglobal}
  | "="                                                 {Leq}
  | "true"                                              {Ltrue}
  | "false"                                             {Lfalse}
  | "if"                                                {Lif}
  | "then"                                              {Lthen}
  | "else"                                              {Lelse}
  | "let"                                               {Llet}
  | "in"                                                {Lin}
  | ";;"                                                {Lend}
  | "unit"                                              {Lunit}
  | ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*        {Lident (Lexing.lexeme lexbuf)}
  | ['0'-'9']+                                          {Linteger (Lexing.lexeme lexbuf)}
