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
type order = M of move | R of rot
					  
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
      match r with
	CW -> { x = - c.z; y = - c.x; z = - c.y }
      | CCW -> { x = - c.y; y = - c.z; z = - c.x }

    let move (c:t) (m: move) : t =
      match m with
      | E -> { x = c.x + 1 ; y = c.y - 1 ; z = c.z }
      | W -> { x = c.x - 1 ; y = c.y + 1 ; z = c.z }
      | SE -> { x = c.x ; y = c.y - 1 ; z = c.z + 1 }
      | SW -> { x = c.x -1 ; y = c.y ; z = c.z + 1 }
  end

(** A unitary cell **) 
module Cell = struct
    type t = cell
    let compare = compare

    let move (cell: t) (m: move) =
      Cube.cell_of_cube (Cube.move (Cube.cube_of_cell cell) m)
      (* match m with *)
      (* | E -> { x = cell.x + 1; y = cell.y } *)
      (* | W -> { x = cell.x - 1; y = cell.y } *)
      (* | SE -> { x = if cell.y mod 2 == 0 then cell.x else cell.x + 1; *)
      (* 		y = cell.y + 1 } *)
      (* | SW -> { x = if cell.y mod 2 == 0 then cell.x - 1 else cell.x; *)
      (* 		y = cell.y + 1 } *)

    let rot (cell: t) (pivot : t) (r: rot) =
      Cube.cell_of_cube (Cube.rot
			   (Cube.cube_of_cell cell)
			   (Cube.cube_of_cell pivot) r)
		    
  end
module CellSet = Set.Make(Cell)


(* Unités, both as an abstract piece as well as positionned over the board relatively to the pivot *)
module Pawn = struct
    type t = {
	cells: CellSet.t
      ;
	pivot: cell
      }
	       
    let move (p: t) (m: move) : t = p

    let rot (p: t) (r: rot) : t = p
	   
end
module Uunit = Pawn
type pawn = Pawn.t

(* platō *)
(* ligne × colonne *)

module Board = struct
type t = bool array array

(** Return whether a cell is empty or not in a board **)
let get (b: t) (c: cell) : bool =
  b.(c.y).(c.x)

(** Set the value of a cell in a board to v **)
let set (b: t) (c: cell) (v: bool) : unit =
  b.(c.y).(c.x) <- v

(** Initialize a board given its dimensions and a list of cells **)
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

type res_Move = Out_Of_Board | Occupied | Fine 

(** A configuration is a board and a currently falling pawn **)
module Config = struct 
  type t = {
    b: board
  ;
    p: pawn
  }

(** Check the validity of the position of a cell inside a board **)
  let valid_cell (c: cell) (b: board): res_Move =
    let heigth = Array.length b in
    let width =  Array.length b.(0) in
    if (c.x < 0 || c.x >= heigth || c.y < 0 || c.y >= width) then Out_Of_Board
    else if (Board.get b c) then Occupied 
    else Fine

(** Check the validity of a candidate board **) 
  let valid (c: t): bool =
    CellSet.fold (fun cell b -> b && (valid_cell cell c.b) == Fine) c.p.cells true 

(** Updates a configuration after an order **) 
  let update (c: t) (o: order): t =
    match o with 
    | M m -> { b = c.b ; p = Pawn.move c.p m }
    | R r -> { b = c.b ; p = Pawn.rot  c.p r }
    
end
