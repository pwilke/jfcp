open Cell
open Jfcp
open Scoring 

(* platō *)
(* ligne × colonne *)

module Board = struct
type t = bool array array

(** Return whether a cell is empty or not in a board **)
let get (b: t) (c: cell) : bool =
 if debug then Format.eprintf "get %a@\n" pp_cell c;
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

(* Height of a board *)
let height (b: t) : int =
  Array.length b

(* Width of a board *)
let width (b: t) : int =
  Array.length b.(0)

(* Fresh copy of a board. You may freely modify your fresh copy. *)
let clone (b: t) : t =
  let h = height b in
  let w = width b in
  Array.init h (fun r ->
    Array.init w (fun c ->
      b.(r).(c)
    )
  )

(** Copy the line n in b to the following one. Non-sensical for n = 0. **)
let fall_step (b: t) (n: int) : unit =
  b.(n) <- b.(n - 1)

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
      Format.fprintf fmt begin
        match b, Some{ x = col; y = row } = pivot with
        | true, true -> "<>"
        | true, false -> "⟨⟩"
        | false, true -> "··"
        | false, false -> "  "
        end
    )
    c;
    Format.fprintf fmt "|@\n"
  )

(** Compute the list of lines which are full inside a board. Could be optimized if needed by casting it over the configuration by projection and testing only for affected lines **)
 let full_lines (b: t): int list =
   snd (Array.fold_left 
     (fun (n,acc) line -> 
      let full = Array.fold_left (fun acc bool -> acc && bool) true b.(n) in  
      if full then (n+1,n::acc) else (n+1, acc)) 
     (0,[]) b)

 let clean_end_of_round (b: t) (score: (int * int) ref) (size: int): unit =
   let l = List.rev (full_lines b) in
   let ls = List.length l in
   let (old, ls_old) = ! score in
   score := (old + Scoring.move_score old size ls ls_old, ls);
   List.iter (fun n -> fall b n) l

end
module B = Board
type board = B.t
