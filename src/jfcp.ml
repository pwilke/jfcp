(* Global parameters *)

let debug = false

(* formatting lists *)
let pp_list hf fmt =
  List.iter (fun k -> hf fmt k)
