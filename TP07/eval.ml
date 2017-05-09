open Expression ;;

exception Eval_error of string ;;

let succ delta = function
| Natural n -> (delta, Natural (n + 1))
| _ -> raise (Eval_error "invalid succ")
;;

let rec eval_step delta = function
(* E-AppAbs *)
| Application (Function (x, _, t), v) when expression_is_value v ->
  (delta, expression_variable x v t)
| Application (NativeFunction (_, _, _, fn), v) when expression_is_value v ->
  fn delta v
| Application (v1, v2) when expression_is_value v1 && expression_is_value v2 ->
  raise (Eval_error ("cannot apply " ^ (expression_to_string v1)))

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
  (Assoc.put x v delta, v)
| Global (x, t) ->
  let (_, t') = eval_step delta t in
  (delta, Global (x, t'))
| Variable "succ" ->
  (delta, NativeFunction ("succ", Natural, Natural, succ))
| Variable var ->
  let v = Assoc.get var delta in
  (delta, v)

(* E-LetIn *)
| Local (x, v, t) when expression_is_value v ->
  let delta' = Assoc.put x v delta in
  let (_, t') = eval_step delta' t in
  (delta, t')
| Local (x, t1, t2) ->
  let (_, t1') = eval_step delta t1 in
  (delta, Local (x, t1', t2))

(* E-If *)
| Cond (Boolean true, t, _) ->
  eval_step delta t
| Cond (Boolean false, _, t) ->
  eval_step delta t
| Cond (c, t, e) when not (expression_is_value c) ->
  let (_, c') = eval_step delta c in
  (delta, Cond (c', t, e))

| Each (a, b) when expression_is_value a ->
  (delta, b)
| Each (a, b) ->
  let (_, a') = eval_step delta a in
  (delta, Each (a', b))

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
