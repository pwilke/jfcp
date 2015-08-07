open Board
open Pawn
open Cell

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
