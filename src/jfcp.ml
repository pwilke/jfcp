(* Global parameters *)

let debug = false

(* formatting ints *)
let pp_int fmt = Format.fprintf fmt "%d"

(* formatting lists *)
let pp_list hf sep fmt =
  List.iter (Format.fprintf fmt "%s%a" sep hf)

(* iterate a function *)
let iter (n: int) (f: 'a -> 'a) (a: 'a) : 'a =
  let rec aux a = function
  | 0 -> a
  | n -> aux (f a) (n - 1)
  in 
  aux a n
