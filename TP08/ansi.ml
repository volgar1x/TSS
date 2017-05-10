type code = Blue | Cyan | Red | Bold | Underline | Reset | Reverse ;;

let string_of_code = function
| Blue -> "34"
| Cyan -> "32"
| Red -> "31"
| Bold -> "1"
| Underline -> "4"
| Reset -> "0"
| Reverse -> "7"
;;

let wrap n str =
  match Sys.os_type with
  | "Win32" -> str
  | _ -> "\027[" ^ (String.concat ";" (List.map string_of_code n)) ^ "m" ^ str ^ "\027[0m"
;;
