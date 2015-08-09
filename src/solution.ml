open Orders

let trivial_symb_of_order (o: order) =
  match o with
  | M E -> "e"
  | M W -> "!"
  | M SE -> "l"
  | M SW -> "i"
  | R CW -> "d"
  | R CCW -> "k"

let trivial_string_of_order_list (ol: order list) =
  String.concat "" (List.map trivial_symb_of_order ol)

(* let rec remove x = function *)
(*   | [] -> failwith "x not in list" *)
(*   | h::t -> if h = x then t else h::(remove x t);; *)

(* let rec prelist a b = match a, b with *)
(*   | [], _ -> true *)
(*   | _, [] -> false *)
(*   | h::t, h'::t' -> h = h' && prelist t t';; *)

(* let rec sublist a b = match a, b with *)
(*   | [], _ -> true *)
(*   | _, [] -> false *)
(*   | h::_, h'::t' -> (h = h' && prelist a b) || sublist a t';; *)
		
let order_of_char c : order =
  if String.contains "p'!03" c then M W
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
		
