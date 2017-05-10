let allsame cmp xs =
  let rec aux x = function
  | [] -> true
  | hd :: tl ->
    cmp x hd && aux x tl
  in

  match xs with
  | [] -> true
  | hd :: tl -> aux hd tl
;;

let branch_first fn xs =
  let rec aux = function
  | [] -> None
  | hd :: tl ->
    begin match fn hd with
    | None -> aux tl
    | (Some _) as res -> res
    end
  in

  aux xs
;;
