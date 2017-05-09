open Parser ;;
open Lexer ;;
open Expression ;;

(* check command-line parameters *)
if (Array.length Sys.argv) <= 1 then
  print_endline ("Usage: " ^ Sys.argv.(0) ^ " [file]")
else
  ()
;;

(* describe where in the code we are *)
let code_loc buf =
  let loc = buf.Lexing.lex_curr_p in
  "line " ^ (string_of_int loc.Lexing.pos_lnum)
;;

(* main evaluation loop *)
let rec loop delta buf =
  try
    let t = Parser.line Lexer.lexer buf in
    print_endline ("> " ^ (expression_to_string t));
    let (delta', t') = Eval.eval delta t in
    print_endline ((expression_to_string t') ^ "\n");
    loop delta' buf
  with
  | Parsing.Parse_error -> print_endline ("Parse error on " ^ (code_loc buf) ^ "\n")
  | Failure msg -> print_endline ("Parse error on " ^ (code_loc buf) ^ ": " ^ msg ^ "\n")
  | Eval.Eval_error reason -> print_endline ("Eval error on " ^ (code_loc buf) ^ ": " ^ reason ^ "\n"); loop delta buf
  | End_of_file -> ()
;;

(* open file and evaluate it *)
let ch = open_in Sys.argv.(1) in
try loop [] (Lexing.from_channel ch)
with _ -> ();
close_in ch
;;
