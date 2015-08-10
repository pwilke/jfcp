(* Global parameters *)

let debug = false
      
let verbose : bool ref = ref false

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

(* Coproducts *)
type ('a, 'b) either =
| Left of 'a
| Right of 'b

let power_phrases =
  ["cthugha", ref false] 

  (* ["cthulhu fhtagn!" ; "yogsothoth" ; "ei!" ;"ia! ia!";"yuggoth"; *)
  (*   "r'lyeh"; "planet 10"] |> *)
  (*   (\* "cthulhu";"bigboote";"conway"; *\) *)
  (*   (\* "cocke";"backus";"hopcroft"; *\) *)
 
  (*   (List.sort *)
  (* 	      (fun a b -> compare (String.length b) (String.length a)) *)
  (* 	       ) |> *)
    
  (*   List.map (fun a -> (a,ref false)) *)
	   
(*ei!, hopcroft *)
