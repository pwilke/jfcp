open Orders
open Cell
open Board
       
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
