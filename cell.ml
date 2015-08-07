open Orders
       
(* Cellule *)
type cell = {
  x: int
  ;
  y: int
}

let pp_cell fmt { x ; y } =
  Format.fprintf fmt "{ %d; %d}" x y

type cube = { x: int; y: int; z: int}
module Cube = struct
    type t = cube
    (* http://www.redblobgames.com/grids/hexagons/#conversions *)
    
    let odd i = if i mod 2 == 0 then 1 else 0

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



(** A unitary cell **) 

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

let cellset_map (f: Cell.t -> Cell.t) (s: CellSet.t) : CellSet.t =
  CellSet.fold (fun elt acc -> CellSet.add (f elt) acc) s CellSet.empty
