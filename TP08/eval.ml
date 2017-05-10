open Expression ;;

exception Eval_error of string ;;

let lambda_succ gamma delta = function
| Natural n -> (delta, Natural (n + 1))
| _ -> raise (Eval_error "invalid succ")
;;

let lambda_pred gamma delta = function
| Natural n -> (delta, Natural (n - 1))
| _ -> raise (Eval_error "invalid pred")
;;

let lambda_iszero gamma delta = function
| Natural n -> (delta, Boolean (n == 0))
| _ -> raise (Eval_error "invalid iszero")
;;


let rec expression_recfunc name ty t =
  let (pty, rty) = match ty with
  | Apply (l, r) -> (l, r)
  | _ -> raise (Eval_error ("bad recursive function signature: " ^ (string_of_type ty)))
  in

  let rec aux gamma delta v =
    let delta' = Assoc.put name self delta in
    let t' = expression_variable name self t in
    eval gamma delta' (Application (t', v))

  and self = NativeFunction (name, pty, rty, aux) in

  self


and eval_step gamma delta = function
(* references *)
| Application (Variable "ref", v) when expression_is_value v ->
  let (_, vty) = Typesystem.type_of_expression gamma v in
  (delta, Ref ("<anon>", vty, ref v))
| Application (Variable "ref", t) ->
  let (_, t') = eval_step gamma delta t in
  (delta, Application (Variable "ref", t'))

(* reference assignment *)
| Assign (var, v) when expression_is_value v ->
  begin match Assoc.get var delta with
  | Ref (_, _, r) -> r := v
  | t -> raise (Eval_error ("cannot assign variable `" ^ var ^ "' which is " ^ (expression_to_string t)))
  end;
  (delta, Unit)
| Assign (var, t) ->
  let (_, t') = eval_step gamma delta t in
  (delta, Assign (var, t'))

(* reference access *)
| Access (Ref (_, _, r)) ->
  (delta, !r)
| Access (v) when expression_is_value v ->
  raise (Eval_error "")
| Access (t) ->
  let (_, t') = eval_step gamma delta t in
  (delta, Access (t'))

(* E-AppAbs *)
| Application (Function (x, _, t), v) when expression_is_value v ->
  (delta, expression_variable x v t)
| Application (NativeFunction (_, _, _, fn), v) when expression_is_value v ->
  fn gamma delta v
| Application (v1, v2) when expression_is_value v1 && expression_is_value v2 ->
  raise (Eval_error ("cannot apply " ^ (expression_to_string v1)))

(* E-App2 *)
| Application (t, v) when expression_is_value v ->
  let (_, t') = eval_step gamma delta t in
  (delta, Application (t', v))

(* E-App1 *)
| Application (t1, t2) ->
  let (_, t2') = eval_step gamma delta t2 in
  (delta, Application (t1, t2'))

(* E-Alias *)
| Global (x, Ref (_, ty, r)) ->
  let r = Ref (x, ty, r) in
  (Assoc.put x r delta, r)
| Global (x, v) when expression_is_value v ->
  (Assoc.put x v delta, v)
| Global (x, t) ->
  let (_, t') = eval_step gamma delta t in
  (delta, Global (x, t'))
| Variable "succ" -> (delta, NativeFunction ("succ", Natural, Natural, lambda_succ))
| Variable "pred" -> (delta, NativeFunction ("pred", Natural, Natural, lambda_pred))
| Variable "iszero" -> (delta, NativeFunction ("pred", Natural, Natural, lambda_iszero))
| Variable var ->
  begin match Assoc.find var delta with
  | None -> raise (Eval_error ("undefined variable " ^ var ^ ": " ^ (Assoc.keys_to_string delta)))
  | Some v -> (delta, v)
  end

(* E-LetVal *)
| Local (x, v, t) when expression_is_value v ->
  (delta, expression_variable x v t)

(* E-Let *)
| Local (x, t1, t2) ->
  let (_, t1') = eval_step gamma delta t1 in
  (delta, Local (x, t1', t2))

(* E-If *)
| Cond (Boolean true, t, _) ->
  eval_step gamma delta t
| Cond (Boolean false, _, t) ->
  eval_step gamma delta t
| Cond (c, t, e) when not (expression_is_value c) ->
  let (_, c') = eval_step gamma delta c in
  (delta, Cond (c', t, e))

(* E-Seq *)
| Each (a, b) when expression_is_value a ->
  (delta, b)
| Each (a, b) ->
  let (_, a') = eval_step gamma delta a in
  (delta, Each (a', b))

(* E-Record *)
| (Record xs) as f when not (expression_is_value f) ->
  let f' = Record (List.map (fun (f, t) ->
    let (_, t') = eval_step gamma delta t in
    (f, t')
  ) xs) in
  (delta, f')

(* E-ProjRcd *)
| Proj (self, f) when expression_is_value self ->
  begin match self with
  | Record xs -> (delta, Assoc.get f xs)
  | t -> raise (Eval_error ("cannot access " ^ f ^ " of " ^ (expression_to_string t)))
  end

(* E-Proj *)
| Proj (self, f) ->
  let (_, self') = eval_step gamma delta self in
  (delta, Proj (self', f))

(* E-CaseInl, E-CaseInr *)
| Case (v, cases) when expression_is_value v ->
  let result = MoreList.branch_first (function
    | VariantCase (f, x, t) ->
      begin match v with
      | Variant (f2, v, _) when String.equal f f2 ->
        Some (expression_variable x v t)
      | _ -> None
      end
    | VariantFallthrough t ->
      let (_, t') = eval_step gamma delta t in
      Some t'
  ) cases in

  begin match result with
  | Some result' -> (delta, result')
  | None -> raise (Eval_error ("case incomplete"))
  end

(* E-Case *)
| Case (t, cases) ->
  let (_, t') = eval_step gamma delta t in
  (delta, Case (t', cases))

(* E-Fix *)
| DefineRecFunc (name, ty, t, rest) ->
  let fn0 = expression_recfunc name ty t in
  print_endline (expression_to_string fn0);
  let fn = expression_variable name fn0 fn0 in
  (delta, expression_variable name fn rest)

| v when expression_is_value v ->
  (delta, v)

| t ->
  raise (Eval_error ("stuck term: " ^ (expression_to_string t)))


and eval gamma delta t =
  let rec aux delta t =
    if expression_is_value t then
      (delta, t)
    else
      let (delta', t') = eval_step gamma delta t in
      aux delta' t'
  in

  aux delta t
;;
