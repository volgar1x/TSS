open Expression ;;

exception Type_error of string ;;

let type_expect a b =
  if type_equal a b then ()
  else raise (Type_error ("expected " ^ (string_of_type b) ^ " but got: " ^ (string_of_type a)))
;;

let rec type_of_expression gamma = function
| Function (x, ty, t) ->
  let gamma' = Assoc.put x ty gamma in
  let (_, tty) = type_of_expression gamma' t in
  (gamma, Apply (ty, tty))

| Cond (c, t, e) ->
  let (_, cty) = type_of_expression gamma c in
  let (_, tty) = type_of_expression gamma t in
  let (_, ety) = type_of_expression gamma e in
  type_expect cty Boolean;
  type_expect tty ety;
  (gamma, tty)

| Application (t1, t2) ->
  let (_, t1ty) = type_of_expression gamma t1 in
  let (_, t2ty) = type_of_expression gamma t2 in
  (match t1ty with
  | Apply (ty, rty) ->
    type_expect t2ty ty;
    (gamma, rty)
  | _ -> raise (Type_error ("cannot apply " ^ (expression_to_string t1))))

| Record xs ->
  let xs' = List.map
    (fun (f, t) ->
      let (_, ty) = type_of_expression gamma t in
      (f, ty)
    )
    xs in
  (gamma, Record xs')

| Proj (self, f) ->
  let (_, ty) = type_of_expression gamma self in
  begin match ty with
  | Record xs -> (gamma, Assoc.get f xs)
  | _ -> raise (Type_error ("cannot access field `" ^ f ^ "' on " ^ (string_of_type ty)))
  end

| Variant (x, t, ty) ->
  (gamma, ty)

| Case (t, cases) ->
  let (_, ty) = type_of_expression gamma t in

  let results = List.map (function
    | VariantCase (f, x, t2) ->
      let xty = match ty with
        | Variant xs ->
          begin match Assoc.find f xs with
          | Some xty -> xty
          | None -> raise (Type_error ("type " ^ (string_of_type ty) ^ " hasnt any variant " ^ f))
          end

        | _ ->
          raise (Type_error ("cannot match on " ^ (string_of_type ty)))
      in
      let (_, t2ty) = type_of_expression (Assoc.put x xty gamma) t2 in
      t2ty
    | VariantFallthrough t2 ->
      let (_, t2ty) = type_of_expression gamma t2 in
      t2ty
  ) cases in

  if not (MoreList.allsame type_equal results) then
    raise (Type_error "");

  begin match results with
  | [] -> (gamma, Unit)
  | hd :: _ -> (gamma, hd)
  end

| DefineRecFunc (x, ty, t, body) ->
  let gamma' = Assoc.put x ty gamma in
  let (_, tty) = type_of_expression gamma' t in
  type_expect tty ty;
  let (_, bodyty) = type_of_expression gamma' body in
  (gamma, bodyty)

| Global (x, t) ->
  let (_, tty) = type_of_expression gamma t in
  (Assoc.put x tty gamma, tty)

| Local (x, t1, t2) ->
  let (_, t1ty) = type_of_expression gamma t1 in
  let gamma' = Assoc.put x t1ty gamma in
  let (_, t2ty) = type_of_expression gamma' t2 in
  (gamma, t2ty)

| Each (a, b) ->
  let (gamma', _) = type_of_expression gamma a in
  type_of_expression gamma' b

| Variable "succ" -> (gamma, Apply (Natural, Natural))
| Variable "pred" -> (gamma, Apply (Natural, Natural))
| Variable "iszero" -> (gamma, Apply (Natural, Boolean))
| Variable x ->
  begin match Assoc.find x gamma with
  | None -> raise (Type_error ("undefined variable " ^ x))
  | Some xty -> (gamma, xty)
  end

| Natural _ -> (gamma, Natural)
| Boolean _ -> (gamma, Boolean)
| Unit -> (gamma, Unit)

| t -> raise (Type_error ("TODO: " ^ (expression_to_string t)))
;;
