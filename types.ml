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
