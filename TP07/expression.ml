type type_ = Boolean
           | Natural
           | Unit
           | Apply of type_ * type_
           | Name of string
           ;;

let type_of_string = function
  | "Bool" -> Boolean
  | "bool" -> Boolean
  | "Nat" -> Natural
  | "Unit" -> Unit
  | n -> Name n
;;

let rec type_equal a b =
  match a with
  | Apply (aa, ab) ->

    (match b with
    | Apply (ba, bb) -> (type_equal aa ba) && (type_equal ab bb)
    | _ -> false)

  | _ ->
    (match b with
    | Apply _ -> false
    | _ -> a == b)
;;

let rec string_of_type = function
  | Boolean -> "Bool"
  | Natural -> "Nat"
  | Unit -> "Unit"
  | Apply (a, b) -> "(" ^ (string_of_type a) ^ " -> " ^ (string_of_type b) ^ ")"
  | Name n -> "^" ^ n
;;

type expression = Variable of string
                | Function of string * type_ * expression
                | NativeFunction of string * type_ * type_ * (((string * expression) list) -> expression -> (((string * expression) list) * expression))
                | Application of expression * expression
                | Global of string * expression
                | Boolean of bool
                | Natural of int
                | Unit
                | Cond of expression * expression * expression
                | Each of expression * expression
                ;;

let rec expression_to_string = function
  | Variable var -> var
  | Function (var, ty, body) -> "λ" ^ var ^  " : " ^ (string_of_type ty) ^ ". " ^ (expression_to_string body)
  | NativeFunction (f, ty, rty, _) -> "λx : " ^ (string_of_type ty) ^ ". [native/" ^ f ^ " : " ^ (string_of_type rty) ^ "]"
  | Application (left, right) -> "(" ^ (expression_to_string left) ^ " " ^ (expression_to_string right) ^ ")"
  | Global (varname, varexpr) -> "let " ^ varname ^ " = " ^ (expression_to_string varexpr) ^ " ;; "
  | Boolean b -> if b then "true" else "false"
  | Natural n -> string_of_int n
  | Unit -> "unit"
  | Cond (c, t, e) -> "if " ^ (expression_to_string c) ^ " then " ^ (expression_to_string t) ^ " else " ^ (expression_to_string e)
  | Each (a, b) -> (expression_to_string a) ^ " ; " ^ (expression_to_string b)
;;

let rec expression_is_value = function
  | Function _ -> true
  | NativeFunction _ -> true
  | Boolean _ -> true
  | Natural _ -> true
  | Unit -> true
  | _ -> false
;;

let expression_variable x v t =
  let rec aux = function
  | Variable x' when String.equal x x' -> v
  | (Function (x', ty, _)) as t1 when String.equal x x' -> t1
  | (Global (x', _)) as t1 when String.equal x x' -> t1

  | Function (x', ty, t) -> Function (x', ty, aux t)
  | Application (t1, t2) -> Application (aux t1, aux t2)
  | Global (x', t) -> Global (x', aux t)
  | Cond (c, t, e) -> Cond (aux c, aux t, aux e)

  | t -> t
  in

  aux t
;;
