exception Assoc_error of string ;;

let rec find key = function
  | [] -> None
  | (k, v) :: tl when String.equal key k -> Some v
  | _ :: tl -> find key tl
;;

let get key delta =
  match find key delta with
  | None -> raise (Assoc_error ("undefined variable `" ^ key ^ "'"))
  | Some v -> v
;;

let put key value xs =
  let rec aux acc = function
    | [] -> (key, value) :: (List.rev acc)
    | (key', _) :: xs when String.equal key key' -> (key, value) :: (List.rev acc) @ xs
    | x :: xs -> aux (x :: acc) xs
  in

  aux [] xs
;;

let to_string ?start:(start="{") ?stop:(stop="}") ?sep:(sep=", ") ?kv:(kv=" = ") f xs =
  start ^
    (String.concat sep (
      List.map (fun (key, value) -> key ^ kv ^ (f value))
               xs
    )) ^
  stop
;;

let keys_to_string xs =
  "[" ^
    (String.concat ", " (
      List.map (fun (key, value) -> key)
               xs
    )) ^
  "]"
;;
