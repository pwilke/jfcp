open Jfcp

(* Cellule *)
type cell = {
  x: int
  ;
  y: int
}

let pp_cell fmt { x ; y } =
  Format.fprintf fmt "{ %d; %d}" x y

(* Directions *)
type move = E | W | SW | SE
type rot = CW | CCW
type order = M of move | R of rot


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
			 

(* platō *)
(* ligne × colonne *)

module Board = struct
type t = bool array array

(** Return whether a cell is empty or not in a board **)
let get (b: t) (c: cell) : bool =
  b.(c.y).(c.x)

(** Set the value of a cell in a board to v **)
let set (b: t) (c: cell) (v: bool) : unit =
  if debug then Format.eprintf "set %a@\n" pp_cell c;
  b.(c.y).(c.x) <- v

(** Initialize a board given its dimensions and a list of cells **)
let init (h: int) (w: int) (l: cell list) : t =
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

(** formatted representation of a board *)
let format ~pivot fmt =
  Array.iteri (fun row c ->
    (* trailing space on odd rows *)
    Format.fprintf fmt (if row land 1 = 1 then "|-" else "|");
    Array.iteri (fun col b ->
      Format.fprintf fmt (if b then "⟨⟩" else if Some{ x = col; y = row } = pivot then "··" else "  ")
    )
    c;
    Format.fprintf fmt "|@\n"
  )

end
module B = Board
type board = B.t

(* Unités, both as an abstract piece as well as positionned over the board relatively to the pivot *)
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

    let move (p: t) (m: move) : t = p

    let rot (p: t) (r: rot) : t = p

  (* Bounding box of a cell set *)
  (* returns (width, height) *)
  let cellset_bb (cells: CellSet.t) =
    CellSet.fold (fun c (w, h) ->
      (max w c.x, max h c.y)
    ) cells (0, 0)
  (* Bounding box of a pawn (including its pivot) *)
  let bb (p: t) =
    cellset_bb (CellSet.add p.pivot p.cells)

  let format fmt (p: t) =
    let (w, h) = bb p in
    let pivot = Some p.pivot in
    let b = Board.init h w (CellSet.elements p.cells) in
    Format.fprintf fmt "%a" (Board.format ~pivot) b
   
end
module Uunit = Pawn
type pawn = Pawn.t

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
    CellSet.fold (fun cell b -> b && (valid_cell cell c.b) == Fine) c.p.Pawn.cells true 

(** Updates a configuration after an order **) 
  let update (c: t) (o: order): t =
    match o with 
    | M m -> { b = c.b ; p = Pawn.move c.p m }
    | R r -> { b = c.b ; p = Pawn.rot  c.p r }
    
end
