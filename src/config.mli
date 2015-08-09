type res_Move = Out_Of_Board | Occupied | Fine
module Config :
  sig
    type t = { b : Board.board; p : Pawn.pawn; }
    val valid_cell : Cell.cell -> Board.board -> res_Move
    val valid : Board.board -> Pawn.pawn -> bool
    val valid_config : t -> bool
    val proj : t -> Board.board
    val update : t -> Orders.order -> t option
    type score_t = int * int * int * bool
    val score : Board.board -> Pawn.pawn -> Orders.order list -> score_t
    val compute_sons :
      Board.board ->
      Pawn.pawn -> (Orders.order * Pawn.pawn) list * Orders.order list
    val equiv : Pawn.pawn -> Pawn.pawn -> bool
    val not_colored : Pawn.pawn list -> Pawn.pawn -> bool
    val walk : t -> Orders.order list
    val init : t -> t
  end
