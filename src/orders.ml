(* Directions *)
type move = E | W | SW | SE
type rot = CW | CCW
type order = M of move | R of rot

				
let string_of_move (m: move) : string =
  match m with
    E -> "E"
  | W -> "W"
  | SW -> "SW"
  | SE -> "SE"

let string_of_rot (r: rot) : string =
  match r with
    CW -> "CW"
  | CCW -> "CCW"
	     
let string_of_order (ol: order) : string =
  match ol with
    M m -> Printf.sprintf "Move %s" (string_of_move m)
  | R r -> Printf.sprintf "Rot %s" (string_of_rot r)


			      
let string_of_list_order (ol: order list) : string =
  List.fold_left (fun acc elt ->
		  Printf.sprintf "%s; %s" acc (string_of_order elt)
		 ) "" ol
