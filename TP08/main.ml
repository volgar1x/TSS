open Parser ;;
open Lexer ;;
open Expression ;;
open Typesystem ;;

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
let rec loop gamma delta buf =
  try
    (* print_endline (Assoc.to_string ~kv:" : " string_of_type gamma); *)
    (* print_endline (Assoc.to_string ~kv:" = " expression_to_string delta); *)
    let t = Parser.line Lexer.lexer buf in
    print_endline ("> " ^ (Str.global_replace (Str.regexp "\n") "\n> " (expression_to_string t)));
    let (gamma', ty) = type_of_expression gamma t in
    print_endline ("Σ " ^ (string_of_type ty));
    let (delta', t') = Eval.eval gamma delta t in
    print_endline ((expression_to_string t') ^ "\n");
    loop gamma' delta' buf
  with
  | Parsing.Parse_error -> print_endline ("Parse error on " ^ (code_loc buf) ^ "\n  `" ^ (Lexing.lexeme buf) ^ "'\n")
  | Failure msg -> print_endline ("Parse error on " ^ (code_loc buf) ^ ": " ^ msg ^ "\n")
  | Eval.Eval_error reason -> print_endline ("Eval error on " ^ (code_loc buf) ^ ": " ^ reason ^ "\n"); loop gamma delta buf
  | Assoc.Assoc_error reason -> print_endline ("Eval error on " ^ (code_loc buf) ^ ": " ^ reason ^ "\n"); loop gamma delta buf
  | Typesystem.Type_error reason -> print_endline ("Type error on " ^ (code_loc buf) ^ ": " ^ reason ^ "\n"); loop gamma delta buf
  | End_of_file -> ()
;;

(* open file and evaluate it *)
let ch = open_in Sys.argv.(1) in
try loop [] [] (Lexing.from_channel ch)
with err -> print_endline ("unexpected error: " ^ (Printexc.to_string err));
close_in ch
;;
