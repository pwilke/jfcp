(* Cellule *)
type cell = {
  x: int
  ;
  y: int
}

module Cell = struct type t = cell let compare = compare end
module CellSet = Set.Make(Cell)

(* Directions *)
type move = E | W | SW | SE
type rot = CW | CCW

(* Unités *)
module Pawn = struct
type t = {
  cells: CellSet.t
  ;
  pivot: cell
}
end
module Uunit = Pawn

(* platō *)
(* ligne × colonne *)

module Board = struct
type t = bool array array

let get (b:t) (c: cell) : bool =
  b.(c.y).(c.x)

let set (b:t) (c: cell) (v: bool) : bool =
  b.(c.y).(c.x) <- v

let fall (b:t) : unit =
  failwith "toto"

end
module B = Board
type board = B.t
