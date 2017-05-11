open Exceptions

type type_ = Boolean
           | Natural
           | Unit
           | Any
           | Nothing
           | Apply of type_ * type_
           | Name of string
           | Record of ((string * type_) list)
           | Variant of ((string * type_) list)
           | Ref of type_
           | Source of type_
           | Sink of type_
           ;;

let type_of_string = function
  | "Bool" -> Boolean
  | "bool" -> Boolean
  | "Nat" -> Natural
  | "Unit" -> Unit
  | "Any" -> Any
  | "Nothing" -> Nothing
  | s -> raise (Type_error ("unknown type `" ^ s ^ "'"))
;;

let rec string_of_type = function
  | Boolean -> "Bool"
  | Natural -> "Nat"
  | Unit -> "Unit"
  | Any -> "Any"
  | Nothing -> "Nothing"
  | Record xs -> Assoc.to_string ~kv:" : " string_of_type xs
  | Variant xs -> Assoc.to_string ~start:"<" ~stop:">" ~kv:" : " ~sep:" | " string_of_type xs
  | Apply ((Apply _) as a, b) -> "(" ^ (string_of_type a) ^ ") -> " ^ (string_of_type b)
  | Apply (a, b) -> (string_of_type a) ^ " -> " ^ (string_of_type b)
  | Name n -> "^" ^ n
  | Ref ((Apply _) as ty) -> "Ref (" ^ (string_of_type ty) ^ ")"
  | Ref ty -> "Ref " ^ (string_of_type ty)
  | Source ty -> "Source " ^ (string_of_type ty)
  | Sink ty -> "Sink " ^ (string_of_type ty)
;;

let issubtype left right =
  let rec aux = function
  | (Record rcd1, Record rcd2) ->
    List.for_all (fun (f1, t1) ->
      List.exists (fun (f2, t2) ->
        String.equal f1 f2 && aux (t2, t1)
      ) rcd1
    ) rcd2

  | (Apply (a, b), Apply (c, d)) ->
    aux (c, a) && aux (b, d)

  | (Variant variant1, Variant variant2) ->
    List.for_all (fun (f1, t1) ->
      List.exists (fun (f2, t2) ->
        String.equal f1 f2 && aux (t1, t2)
      ) variant2
    ) variant1

  | (Ref a, Ref b)    -> aux (a, b) && aux (b, a)
  | (Ref a, Source b) -> aux (a, b)
  | (Ref a, Sink b)   -> aux (b, a)

  | (_, Any) -> true
  | (_, Nothing) -> false
  | (Natural, Natural) -> true
  | (Boolean, Boolean) -> true
  | (Unit, Unit) -> true
  | _ -> false
  in

  aux (left, right)
;;

let type_equal left right = issubtype left right && issubtype right left;;

type expression = Variable of string
                | Function of string * type_ * expression
                | NativeFunction of string * type_ * type_ * (gamma -> delta -> expression -> (delta * expression))
                | DefineRecFunc of string * type_ * expression * expression
                | Application of expression * expression
                | Global of string * expression
                | Local of string * expression * expression
                | Boolean of bool
                | Natural of int
                | Unit
                | Cond of expression * expression * expression
                | Each of expression * expression
                | Record of ((string * expression) list)
                | Proj of expression * string
                | Variant of string * expression * type_
                | Case of expression * (variant list)
                | Assign of expression * expression
                | Access of expression
                | Ref of string * type_ * (expression ref)

and variant = VariantCase of string * string * expression
            | VariantFallthrough of expression

and delta = (string * expression) list
and gamma = (string * type_) list
;;

let rec string_of_expression = function
  | Variable var -> var
  | Function (var, ty, body) -> "λ" ^ var ^  " : " ^ (string_of_type ty) ^ ". " ^ (string_of_expression body)
  | NativeFunction (f, ty, rty, _) -> "λx : " ^ (string_of_type ty) ^ ". [native/" ^ f ^ " : " ^ (string_of_type rty) ^ "]"
  | DefineRecFunc (x, ty, t, rest) -> "letrec " ^ x ^ " : " ^ (string_of_type ty) ^ " = " ^ (string_of_expression t) ^ " in " ^ (string_of_expression rest)
  | Application ((Function _) as left, ((Application _) as right)) -> "(" ^ (string_of_expression left) ^ ") (" ^ (string_of_expression right) ^ ")"
  | Application ((Function _) as left, right) -> "(" ^ (string_of_expression left) ^ ") " ^ (string_of_expression right)
  | Application (left, ((Application _) as right)) -> (string_of_expression left) ^ " (" ^ (string_of_expression right) ^ ")"
  | Application (left, right) -> (string_of_expression left) ^ " " ^ (string_of_expression right)
  | Global (varname, varexpr) -> "let " ^ varname ^ " = " ^ (string_of_expression varexpr) ^ " ;; "
  | Local (varname, varexpr, body) -> "let " ^ varname ^ " = " ^ (string_of_expression varexpr) ^ " in\n" ^ (string_of_expression body)
  | Boolean b -> if b then "true" else "false"
  | Natural n -> string_of_int n
  | Unit -> "unit"
  | Cond (c, t, e) -> "if " ^ (string_of_expression c) ^ " then " ^ (string_of_expression t) ^ " else " ^ (string_of_expression e)
  | Each (a, b) -> (string_of_expression a) ^ ";\n" ^ (string_of_expression b)
  | Record xs -> Assoc.to_string string_of_expression xs
  | Proj (self, f) -> (string_of_expression self) ^ "." ^ f
  | Variant (f, t, ty) -> "<" ^ f ^ " = " ^ (string_of_expression t) ^ "> as " ^ (string_of_type ty)
  | Assign (var, value) -> (string_of_expression var) ^ " := " ^ (string_of_expression value)
  | Access (var) -> "!" ^ (string_of_expression var)
  | Ref (name, ty, slot) -> name ^ "[" ^ (string_of_expression !slot) ^ "] : " ^ (string_of_type (Ref ty))

  | Case (t, cases) ->
    "case " ^ (string_of_expression t) ^ " of " ^
      (String.concat " | "
        (List.map
          (function VariantCase (f, x, t2) -> "<" ^ f ^ " = " ^ x ^ "> => " ^ (string_of_expression t2)
                  | VariantFallthrough t2 -> "_ => " ^ (string_of_expression t2))
          cases))
;;

let rec verbose_string_of_expression = function
  | Variable var -> "Variable(" ^ var ^ ")"
  | Function (var, ty, body) -> "Function(" ^ var ^ ", " ^ (string_of_type ty) ^ ", " ^ (verbose_string_of_expression body) ^ ")"
  | NativeFunction (f, ty, rty, _) -> "NativeFunction(" ^ f ^ ", " ^ (string_of_type ty) ^ ", " ^ (string_of_type rty) ^ ", _)"
  | DefineRecFunc (x, ty, t, rest) -> "DefineRecFunc(" ^ x ^ ", " ^ (string_of_type ty) ^ ", " ^ (verbose_string_of_expression t) ^ ", " ^ (verbose_string_of_expression rest) ^ ")"
  | Application (left, right) -> "Application(" ^ (verbose_string_of_expression left) ^ ", " ^ (verbose_string_of_expression right) ^ ")"
  | Global (varname, varexpr) -> "Global(" ^ varname ^ ", " ^ (verbose_string_of_expression varexpr) ^ ")"
  | Local (varname, varexpr, body) -> "Local(" ^ varname ^ ", " ^ (verbose_string_of_expression varexpr) ^ ", " ^ (verbose_string_of_expression body) ^ ")"
  | Boolean b -> "Boolean(" ^ (if b then "true" else "false") ^ ")"
  | Natural n -> "Natural(" ^ (string_of_int n) ^ ")"
  | Unit -> "Unit"
  | Cond (c, t, e) -> "Cond(" ^ (verbose_string_of_expression c) ^ ", " ^ (verbose_string_of_expression t) ^ ", " ^ (verbose_string_of_expression e) ^ ")"
  | Each (a, b) -> "Each(" ^ (verbose_string_of_expression a) ^ ", " ^ (verbose_string_of_expression b) ^ ")"
  | Record xs -> "Record(" ^ (Assoc.to_string verbose_string_of_expression xs) ^ ")"
  | Proj (self, f) -> "Proj(" ^ (verbose_string_of_expression self) ^ ", " ^ f ^ ")"
  | Variant (f, t, ty) -> "Variant(" ^ f ^ ", " ^ (verbose_string_of_expression t) ^ ", " ^ (string_of_type ty) ^ ")"
  | Assign (var, value) -> "Assign(" ^ (verbose_string_of_expression value) ^ ", " ^ (verbose_string_of_expression value) ^ ")"
  | Access (var) -> "Access(" ^ (verbose_string_of_expression var) ^ ")"
  | Ref (name, ty, slot) -> "Ref(" ^ name ^ ", " ^ (string_of_type ty) ^ ", !" ^ (verbose_string_of_expression !slot) ^ ")"
  | Case (t, cases) -> "Case(" ^ (verbose_string_of_expression t) ^ ", [" ^
                      (String.concat ", " (List.map (function
                        | VariantCase (f, x, t2) -> "VariantCase(" ^ f ^ ", " ^ x ^ ", " ^ (verbose_string_of_expression t2) ^ ")"
                        | VariantFallthrough (t2) -> "VariantFallthrough(" ^ (verbose_string_of_expression t2) ^ ")"
                       ) cases)) ^ "])"
;;

let rec expression_is_value = function
  | Function _ -> true
  | NativeFunction _ -> true
  | Boolean _ -> true
  | Natural _ -> true
  | Unit -> true
  | Ref _ -> true
  | Record xs -> List.for_all (fun (k, v) -> expression_is_value v) xs
  | Variant (_, t, _) -> expression_is_value t
  | _ -> false
;;

let expression_variable x v t =
  let rec aux = function
  | Variable x' when String.equal x x' -> v
  | (Function (x', ty, _)) as t1 when String.equal x x' -> t1
  | (Global (x', _)) as t1 when String.equal x x' -> t1
  | (Local (x', _, _)) as t1 when String.equal x x' -> t1

  | Function (x', ty, t) -> Function (x', ty, aux t)
  | Application (t1, t2) -> Application (aux t1, aux t2)
  | Global (x', t) -> Global (x', aux t)
  | Local (x', t1, t2) -> Local (x', aux t1, aux t2)
  | Cond (c, t, e) -> Cond (aux c, aux t, aux e)
  | Record xs -> Record (List.map (fun (k, v) -> (k, aux v)) xs)
  | Variant (f, t, ty) -> Variant (f, aux t, ty)
  | Case (t, cases) -> Case (aux t, List.map (function
                                              | VariantCase (f, x, t2) -> VariantCase (f, x, aux t2)
                                              | VariantFallthrough t2 -> VariantFallthrough (aux t2))
                                             cases)
  | Proj (self, f) -> Proj (aux self, f)
  | Each (t1, t2) -> Each (aux t1, aux t2)
  | Assign (x, t) -> Assign (aux x, aux t)
  | Access (t) -> Access (aux t)

  | t -> t
  in

  aux t
;;
