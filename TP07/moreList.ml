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
