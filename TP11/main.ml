open Parser
open Lexer
open Exceptions
open Expression
open Typesystem
open Ansi;;

(* check command-line parameters *)
if (Array.length Sys.argv) <= 1 then
  (print_endline ("Usage: " ^ (Ansi.wrap [Underline] Sys.argv.(0)) ^ " [file]");
  exit 1)
else
  ()
;;

(* describe where in the code we are *)
let code_loc buf =
  let loc = buf.Lexing.lex_curr_p in
  Ansi.wrap [Underline] ("line " ^ (string_of_int loc.Lexing.pos_lnum))
;;

(* main evaluation loop *)
let rec loop gamma delta buf =
  try
    (* print_endline (Assoc.to_string ~kv:" : " string_of_type gamma); *)
    (* print_endline (Assoc.to_string ~kv:" = " string_of_expression delta); *)
    let t = Parser.line Lexer.lexer buf in
    print_endline ((Ansi.wrap [Bold; Blue] ">> ") ^ (Str.global_replace (Str.regexp "\n") ("\n" ^ ((Ansi.wrap [Bold; Blue] ">> "))) (string_of_expression t)));
    let (gamma', ty) = type_of_expression gamma t in
    print_endline ((Ansi.wrap [Bold; Cyan] "ΣΣ ") ^ (string_of_type ty));
    let (delta', t') = Eval.eval gamma delta t in
    print_endline ((string_of_expression t') ^ "\n");
    loop gamma' delta' buf
  with
  | Parsing.Parse_error -> print_endline ((Ansi.wrap [Red] "Parse error") ^ " on " ^ (code_loc buf) ^ ":  `" ^ (Lexing.lexeme buf) ^ "'\n")
  | Failure msg -> print_endline ((Ansi.wrap [Red] "Parse error") ^ " on " ^ (code_loc buf) ^ ": " ^ msg ^ "\n")
  | Eval_error reason -> print_endline ((Ansi.wrap [Red] "Eval error") ^ " on " ^ (code_loc buf) ^ ": " ^ reason ^ "\n"); loop gamma delta buf
  | Type_error reason -> print_endline ((Ansi.wrap [Red] "Type error") ^ " on " ^ (code_loc buf) ^ ": " ^ reason ^ "\n"); loop gamma delta buf
  | End_of_file -> ()
;;

(* open file and evaluate it *)
let ch = open_in Sys.argv.(1) in
try loop [] [] (Lexing.from_channel ch)
with err -> print_endline ("unexpected error: " ^ (Printexc.to_string err));
close_in ch
;;
