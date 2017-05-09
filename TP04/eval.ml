open Expression ;;

exception Eval_error of string ;;

type delta = (string * expression) list ;;

let delta_get key delta =
  try
    List.assoc key delta
  with Not_found ->
    raise (Eval_error ("undefined variable `" ^ key ^ "'"))
;;

let delta_put key value xs =
  let rec aux acc = function
    | [] -> (key, value) :: (List.rev acc)
    | (key', _) :: xs when String.equal key key' -> (key, value) :: (List.rev acc) @ xs
    | x :: xs -> aux (x :: acc) xs
  in

  aux [] xs
;;

let delta_to_string f xs =
  "{" ^
    (String.concat ", " (
      List.map (fun (key, value) -> key ^ ": " ^ (f value))
               xs
    )) ^
  "}"
;;

let delta_keys_to_string xs =
  "[" ^
    (String.concat ", " (
      List.map (fun (key, value) -> key)
               xs
    )) ^
  "]"
;;

let rec eval_step delta = function
(* E-AppAbs *)
| Application (Function (x, t), v) when expression_is_value v ->
  (delta, expression_variable x v t)

(* E-App2 *)
| Application (t, v) when expression_is_value v ->
  let (_, t') = eval_step delta t in
  (delta, Application (t', v))

(* E-App1 *)
| Application (t1, t2) ->
  let (_, t2') = eval_step delta t2 in
  (delta, Application (t1, t2'))

(* E-Alias *)
| Global (x, v) when expression_is_value v ->
  (delta_put x v delta, v)
| Global (x, t) ->
  let (_, t') = eval_step delta t in
  (delta, Global (x, t'))
| Variable var ->
  let v = delta_get var delta in
  (delta, v)

| v when expression_is_value v ->
  (delta, v)

| t ->
  raise (Eval_error ("stuck term: " ^ (expression_to_string t)))
;;

let eval delta t =
  let rec aux delta t =
    if expression_is_value t then
      (delta, t)
    else
      let (delta', t') = eval_step delta t in
      aux delta' t'
  in

  aux delta t
;;
