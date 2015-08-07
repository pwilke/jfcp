(* Global parameters *)

let debug = false

(* formatting ints *)
let pp_int fmt = Format.fprintf fmt "%d"

(* formatting lists *)
let pp_list hf sep fmt =
  List.iter (Format.fprintf fmt "%s%a" sep hf)
