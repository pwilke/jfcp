open Jfcp
open Board
open Pawn
open Orders

type input_t = {
id: int;
width: int;
height: int;
size: int;
pawns: int -> Pawn.t;
board: Board.t;
length: int;
seeds: int list;
}

(** formatting of an input *)
let pp_input fmt { id ; height ; width ; board ; length ; seeds ; size ; pawns } =
Format.fprintf fmt "id: %d; w: %d; h: %d\nl: %d; s: %a@\n%a"
		id width height
		length (pp_list pp_int ";") seeds
		(Board.format ~pivot:None) board
;
for i = 0 to size - 1 do
    Format.fprintf fmt "%a" Pawn.format (pawns i)
done

(* Output format *)
(* [ { "problemId": number   /* The `id` of the game configuration */ *)
(*   , "seed":      number   /* The seed for the particular game */ *)
(*   , "tag":       string   /* A tag for this solution. */ *)
(*   , "solution":  Commands *)
(*   } *)
(* ] *)

type output_t =
{ pb_id: int;
seed: int;
tag: string;
solution: order list }

let to_jason { pb_id; seed; tag; solution } =
let s = Solution.trivial_string_of_order_list solution in
`O [("problemId", `Float (float pb_id)) ; ("seed", `Float (float seed)) ; ("tag", `String tag) ; ("solution", `String s)]


let pp_output fmt { pb_id; seed; tag; solution } =
Format.fprintf fmt "id: %d; seed: %d; tag: %s; \n solution: %s\n"
		pb_id seed tag
		(string_of_list_order solution)

