module Board :
  sig
    type t
    val get : t -> Cell.cell -> bool
    val set : t -> Cell.cell -> bool -> unit
    val init : int -> int -> Cell.cell list -> t
    val height : t -> int
    val width : t -> int
    val clone : t -> t
    val fall : t -> int -> unit
    val format :
      pivot:Cell.cell option -> Format.formatter -> t -> unit
    val full_lines : t -> int list
    val clean_end_of_round : t -> (int * int) ref -> int -> unit
  end
type board = Board.t
