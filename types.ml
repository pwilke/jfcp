(* Cellule *)
type cell = {
  x: int
  ;
  y: int
}

let odd i = if i mod 2 == 0 then 1 else 0

(* Directions *)
type move = E | W | SW | SE
type rot = CW | CCW
					  
type cube = { x: int; y: int; z: int}
module Cube = struct
    type t = cube
    (* http://www.redblobgames.com/grids/hexagons/#conversions *)
    
    let cube_of_cell (h: cell) : t =
      let xx = h.x - (h.y - (odd h.y)) / 2 in
      let zz = h.y in
      { x = xx; z = zz; y = -xx-zz }

    let cell_of_cube (c: t) : cell =
      { y = c.z;
	x = c.x + (c.z - (odd c.z)) / 2 }

    let rot (c: t) (pivot: t) (r: rot) : t =
      let c = { x = c.x - pivot.x;
		y = c.y - pivot.y;
		z = c.z - pivot.z }
      in
      let c' = match r with
	  CW -> { x = - c.z; y = - c.x; z = - c.y }
	| CCW -> { x = - c.y; y = - c.z; z = - c.x }
      in
      { x = c'.x + pivot.x;
	y = c'.y + pivot.y;
	z = c'.z + pivot.z } 

    let move (c:t) (m: move) : t =
      match m with
      | E -> { x = c.x + 1 ; y = c.y - 1 ; z = c.z }
      | W -> { x = c.x - 1 ; y = c.y + 1 ; z = c.z }
      | SE -> { x = c.x ; y = c.y - 1 ; z = c.z + 1 }
      | SW -> { x = c.x -1 ; y = c.y ; z = c.z + 1 }

    let string_of_cube (c: t) : string =
      Printf.sprintf "[%d,%d,%d]" c.x c.y c.z
		
  end

module Cell = struct
    type t = cell
    let compare = compare

    let move (cell: t) (m: move) =
      Cube.cell_of_cube (Cube.move (Cube.cube_of_cell cell) m)

    let rot (cell: t) (pivot : t) (r: rot) =
      Cube.cell_of_cube (Cube.rot
			   (Cube.cube_of_cell cell)
			   (Cube.cube_of_cell pivot) r)

    let string_of_cell (c: t) : string =
      Printf.sprintf "[%d,%d]" c.x c.y
		    
  end
module CellSet = Set.Make(Cell)

(* let _ = *)
(*   let pivot = {x = 2; y = 4} in *)
(*   let c = { x = 3; y = 2} in *)
(*   Printf.printf "rotating %s around %s CW = %s\n" *)
(* 		(Cell.string_of_cell c) *)
(* 		(Cell.string_of_cell pivot) *)
(* 		(Cell.string_of_cell *)
(* 		   (Cell.rot c pivot CW)) *)

			 

let cellset_map (f: Cell.t -> Cell.t) (s: CellSet.t) : CellSet.t =
  CellSet.fold (fun elt acc -> CellSet.add (f elt) acc) s CellSet.empty
			 
(* Unités *)
module Pawn = struct
    type t = {
	cells: CellSet.t
      ;
	pivot: cell
      }

    let map_t (p: t) (f: Cell.t -> Cell.t) : t =
      {
	cells = cellset_map f p.cells;
	pivot = f p.pivot
      }
	       
    let move (p: t) (m: move) : t =
      map_t p (fun c -> Cell.move c m)

    let rot (p: t) (r: rot) : t = 
      map_t p (fun c -> Cell.rot c p.pivot r)
	   
end
module Uunit = Pawn

(* platō *)
(* ligne × colonne *)

module Board = struct
type t = bool array array

let get (b: t) (c: cell) : bool =
  b.(c.y).(c.x)

let set (b: t) (c: cell) (v: bool) : unit =
  b.(c.y).(c.x) <- v

let init (l: cell list) (h: int) (w: int) : t =
  let b = Array.init h (fun _ -> Array.make w false) 
  in  
  List.iter (fun c -> set b c true) l;
  b

(** Copy the line n in b to the following one. Non-sensical for n = height - 1. **)
let fall_step (b: t) (n: int) : unit =
  b.(n + 1) <- b.(n)

(** Set the line n to false **)
let clean_line (b: t) (n: int) : unit =
  b.(n) <- (Array.make (Array.length b.(0)) false) 

(** erase the n-th line, compute the new state of the board **) 
let fall (b:t) (n: int) : unit =
  for k = n downto 1 do
    fall_step b k
  done; 
  clean_line b 0
 
end
module B = Board
type board = B.t
