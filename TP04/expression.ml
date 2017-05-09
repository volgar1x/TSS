type expression = Variable of string
                | Function of string * expression
                | Application of expression * expression
                | Global of string * expression
                ;;

let rec expression_to_string = function
  | Variable var -> var
  | Function (var, body) -> "λ" ^ var ^  ". " ^ (expression_to_string body)
  | Application (left, right) -> "(" ^ (expression_to_string left) ^ " " ^ (expression_to_string right) ^ ")"
  | Global (varname, varexpr) -> "let " ^ varname ^ " = " ^ (expression_to_string varexpr) ^ " ;; "
;;

let rec expression_is_value = function
  | Function _ -> true
  | _ -> false
;;

let expression_variable x v t =
  let rec aux = function
  | Variable x' when String.equal x x' -> v
  | (Function (x', _)) as t1 when String.equal x x' -> t1
  | (Global (x', _)) as t1 when String.equal x x' -> t1

  | Function (x', t) -> Function (x', aux t)
  | Application (t1, t2) -> Application (aux t1, aux t2)
  | Global (x', t) -> Global (x', aux t)

  | t -> t
  in

  aux t
;;
