open Jfcp
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
    try
      if (Board.get b c)
      then Occupied 
      else Fine
    with _ -> Out_Of_Board

(** Check the validity of a candidate board **) 
  let valid (b: board) (p: pawn): bool =
    CellSet.fold (fun cell bool -> bool && (valid_cell cell b) == Fine) p.Pawn.cells true 

  let valid_config (c: t) : bool =
    valid c.b c.p
		 
(** Updates a configuration after an order **) 
  let update (c: t) (o: order): t option =
    let np = 
      match o with 
      | M m ->  Pawn.move c.p m
      | R r ->  Pawn.rot  c.p r
    in
    if (valid c.b np) then Some { b = c.b ; p = np } else None

  type score_t = int * int
							    
  let score (b: board) (p: pawn): score_t =
    (CellSet.fold (fun c n -> max n c.y) p.Pawn.cells 0,
     CellSet.fold (fun c n -> min n c.y) p.Pawn.cells max_int)
							    
  let compute_sons (b: board) (p: pawn) : (order * pawn) list * order list =
    let olds = 
      (List.map (fun order ->
		 (order, update { b; p } order)
		)
		[M E; M W; M SW; M SE; R CW ; R CCW])
    in
    let rec f (pl: (order* t option) list) acc =
      let (acc1,acc2) = acc in
      match pl with
	[] -> acc
      | (o, None)::r -> f r (acc1,o::acc2)
      | (o, Some p)::r -> f r ((o,p.p)::acc1,acc2)
    in
    f olds ([],[])

  let equiv (p: pawn) (q: pawn) : bool =
    p.Pawn.pivot = q.Pawn.pivot && CellSet.equal p.Pawn.cells q.Pawn.cells
	     
  let not_colored (colored: pawn list) (p: pawn) : bool =
    List.exists (equiv p) colored = false
	     
  let walk (c: t) : order list =
    let b = c.b in
    let rec aux (cur: order list) (best: order list) (bestscore: score_t) (colored: pawn list) (pl: pawn list) : (order list * score_t * pawn list) =
      begin
	match pl with
	  [] -> (best,bestscore,colored)
	| p::r ->
	   if debug then Format.printf "%a@\n%d." Pawn.format p (List.length colored);
	   if not_colored colored p = false then aux cur best bestscore colored r
	   else 
	   let colored = p::colored in
	   let (sons_success,sons_failure) = compute_sons b p in
	   let best, bestscore = 
	     begin
	       match sons_failure with
	       | a::sf ->
		  let s = score b p in
		  if s > bestscore
		  then (a::cur,s)
		  else (best,bestscore)
	       | [] -> (best,bestscore)
	     end
	   in 
	   let (ol,sc,col) = List.fold_left (fun (ol,sc,col) (o,p') ->
					     aux (o::cur) ol sc col [p']
					    )
					    (best,bestscore,colored)
					    sons_success in
	   aux cur ol sc col r
      end      
    in
    let (best, bestscore, colored) = aux [] [] (0,0) [] [c.p] in
    List.rev best

 let proj (c: t): board =
   let b = Board.clone c.b in
   CellSet.iter (fun cell -> Board.set b cell true) c.p.Pawn.cells;
   b

 let init (c: t): t =
   let (b, p) = (c.b, c.p) in
   let length = Array.length b.(0) in
   let (lm, rm) = CellSet.fold (fun cell (l,r) -> (min l cell.x, max r cell.x)) p.Pawn.cells (length, 0) in
   let d = rm - lm in
   let t = length / 2 - d / 2 - lm in
   let p = if t > 0
	   then iter t (fun p -> Pawn.move p E) p
	   else iter (-t) (fun p -> Pawn.move p W) p in
   { b ; p }

end
