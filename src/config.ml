open Board
open Pawn
open Cell
open Orders

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
  let valid (b: board) (p: pawn): bool =
    CellSet.fold (fun cell bool -> bool && (valid_cell cell b) == Fine) p.Pawn.cells true 

(** Updates a configuration after an order **) 
  let update (c: t) (o: order): t option =
    let np = 
      match o with 
      | M m ->  Pawn.move c.p m
      | R r ->  Pawn.rot  c.p r
    in
    if (valid c.b np) then Some { b = c.b ; p = np } else None

  let score (b: board) (p: pawn): int =
    CellSet.fold (fun c n -> max n c.y) p.Pawn.cells 0 

 (** Assuming a pawn which is a rotation of the one in the configuration, we compute if possible a path from the starting position to p **) 
  let get_path (c: t) (p: pawn): (order list) option =
    
    Some []
 
end
