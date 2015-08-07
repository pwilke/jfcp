(* Directions *)
type move = E | W | SW | SE
type rot = CW | CCW
type order = M of move | R of rot
