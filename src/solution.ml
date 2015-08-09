open Jfcp
open Orders

let trivial_symb_of_order (o: order) =
  match o with
  | M E -> "b"
  | M W -> "p"
  | M SE -> "l"
  | M SW -> "a"
  | R CW -> "d"
  | R CCW -> "k"

let trivial_string_of_order_list (ol: order list) =
  String.concat "" (List.map trivial_symb_of_order ol)
		
let order_of_char c : order =
  if String.contains "p'!.03" c then M W
  else if String.contains "bcefy2" c then M E
  else if String.contains "aghij4" c then M SW
  else if String.contains "lmno 5" c then M SE
  else if String.contains "dqrvz1" c then R CW
  else if String.contains "kstuwx" c then R CCW
  else failwith "incorrect character"
		
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

let implode l =
  let res = String.create (List.length l) in
  let rec imp i = function
  | [] -> res
  | c :: l -> res.[i] <- c; imp (i + 1) l in
  imp 0 l;;

let order_list_of_string (s: string) : order list =
  List.map order_of_char (explode s)


let replace_power (pow: string) (s : string) : string =
  let pow_ord = order_list_of_string pow in
  let pow_ord_triv = trivial_string_of_order_list pow_ord in
  Str.global_replace (Str.regexp pow_ord_triv) pow s

let replace_powers (pow: string list) (ol: order list) : string =
  let s = trivial_string_of_order_list ol in
  List.fold_left (fun acc p -> replace_power p acc) s pow

let contain_wop (path: order list): bool =
  let s = trivial_string_of_order_list path in
  Format.printf "%s@." s;
 let wops = ["ei!";"ia! ia!";"yuggoth"; "r'lyeh"] in
  List.fold_left 
    (fun b pow -> 
     let pow_ord = order_list_of_string pow in
     let pow_ord_triv = trivial_string_of_order_list pow_ord in
     Format.printf "%s@." pow_ord_triv;
     b || Str.string_match (Str.regexp pow_ord_triv) s 0) false wops

let _ =
  Printf.printf "%B\n" (contain_wop [ M W; M E; M SW; M W; M E; M SW; M W ])
		     
