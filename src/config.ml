open Jfcp
open Board
open Pawn
open Cell
open Orders
open Solution

type res_Move = Out_Of_Board | Occupied | Fine 

(** A configuration is a board and a currently falling pawn **)
       
module Config = struct 
  type t = {
    b: board
    ;
    p: pawn
  }

(** Compute the horizontal component of the bounded box of a pawn given the horizontal constraints of the board **)
  let horizontal_bound (p: pawn) (boundr: int) (boundl: int): int * int = 
    CellSet.fold (fun cell (l,r) -> (min l cell.x, max r cell.x)) p.Pawn.cells (boundr, boundl)

(** Compute the vertical component of the bounded box of a pawn given the horizontal constraints of the board **)
  let vertical_bound (p: pawn) (boundl: int) (boundh: int): int * int =
    CellSet.fold (fun cell (t,b) -> (min t cell.y, max b cell.y)) p.Pawn.cells (boundl, boundh)

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

  let proj (c: t): board =
    let b = Board.clone c.b in
    CellSet.iter (fun cell -> Board.set b cell true) c.p.Pawn.cells;
    b 

(** Updates a configuration after an order **) 
  let update (c: t) (o: order): t option =
    let np = 
      match o with 
      | M m ->  Pawn.move c.p m
      | R r ->  Pawn.rot  c.p r
    in
    if (valid c.b np) then Some { b = c.b ; p = np } else None

  type score_t = int 

  (** Given a board and a pawn, computes the amount of holes the pawn might cover if place over here **)
  let cover_holes (b: board) (p: pawn): int =
    let width = Board.width b in
    let height = Board.height b in
    let (hmin, hmax) = horizontal_bound p (width - 1) 0 in
    let (_, vmax) = vertical_bound p (height - 1) 0 in
    let res = ref 0 in
    for i = hmin to hmax do
      if (vmax < height - 1) then
        (if not(Board.get b { x = i; y = vmax + i }) then res := !res + 1 else ())
    done;
    !res
 
   (** Given a board and a pawn, computes the amount of holes the pawn might cover if place over here **)
  let cover_holes' (b: board) (p: pawn): int =
    let width = Board.width b in
    let height = Board.height b in
    let (hmin, hmax) = horizontal_bound p (width - 1) 0 in
    let (vmin, vmax) = vertical_bound p (height - 1) 0 in
    let res = ref 0 in
    for i = hmin to hmax do
      for j = vmin to vmax + 1 do
	if (j < height) then
          (if Board.get b { x = i; y = j } then 
	     for k = j + 1 to vmax + 1 do
               if k < height && not(Board.get b { x = i; y = k }) then res := !res + 1 done)
      done
    done;
    !res
 
  let score (b: board) (p: pawn): score_t =
    let projected = (proj {b ; p} ) in
    (List.length (Board.full_lines projected)) * 100_000_000 
    - (cover_holes' projected p) * 10 
    + (CellSet.fold (fun c n -> max n c.y) p.Pawn.cells 0) * 100_00 
    + (CellSet.fold (fun c n -> min n c.y) p.Pawn.cells max_int) * 100 
 							    
  let compute_sons (b: board) (p: pawn) : (order * pawn) list * order list =
    let olds = 
      (List.rev_map (fun order ->
		 (order, update { b; p } order)
		)
		[ R CCW; R CW; M SE; M SW; M W; M E ])
    in
    let rec f (pl: (order* t option) list) acc =
      let (acc1,acc2) = acc in
      match pl with
	[] -> acc
      | (o, None)::r -> f r (acc1,o::acc2)
      | (o, Some p)::r -> f r ((o,p.p)::acc1,acc2)
    in
    f olds ([],[])

  let is_colored (colored: PawnSet.t) (p: pawn) : bool =
    PawnSet.mem p colored

  (* walk_ol c colored ol tries to move as dictated by ol.
   * returns None if move is invalid
   * returns Some(c, col) if resulting config is c, and new set of colored col *)
  let rec walk_ol (c: t) (colored: PawnSet.t) (ol: order list) : (t * PawnSet.t) option =
    match ol with
      [] -> Some (c, colored)
    | o::r ->
       match update c o with
  	 None -> None
       | Some c' ->
	  let ps = PawnSet.add c.p colored in
	  if PawnSet.mem c'.p ps
	  then None
	  else walk_ol c' ps r

  let try_walk_wop wop (c: t) (colored: PawnSet.t) : (t*PawnSet.t*order list*bool) =
    let prefix = Solution.order_list_of_string wop in 
    match walk_ol c colored prefix with
      Some (c,colored) -> (c,colored,prefix,true)
    | None -> (c,colored,[],false)
		       

  let try_walk_wops wops (c: t) (colored: PawnSet.t) : (t*PawnSet.t*order list*bool) =
    List.fold_left
      (fun (c, colored, path,succ) wop ->
       let (c, colored, p, s) = try_walk_wop wop c colored in
       (c, colored, path @ p, succ || s)
      )
      (c,colored,[], false)
      wops


  let rec rep_try_walk_wops wops (c: t) (colored: PawnSet.t) : (t*PawnSet.t*order list) =
    match try_walk_wops wops c colored with
      (c,colored,path,false) -> (c,colored,path)
    | (c,colored,path,true) ->
       match rep_try_walk_wops wops c colored with
	 (cc,col,ppath) -> (cc,col, path@ppath)

		
      
  let walk (c: t) (colored: PawnSet.t) : order list * score_t =
    let (c,colored,curpath) = rep_try_walk_wops (List.map fst power_phrases) c colored in
    let b = c.b in
    let rec aux (cur: order list) (best: order list) (bestscore: score_t) (colored: PawnSet.t) (pl: pawn list) : (order list * score_t * PawnSet.t) =
      begin
	match pl with
	  [] -> (best,bestscore,colored)
	| p::r ->
	   if debug then Format.printf "%a@\n%d." Pawn.format p (PawnSet.cardinal colored);
	   if is_colored colored p then aux cur best bestscore colored r
	   else 
	     let colored = PawnSet.add p colored in
	     let (sons_success,sons_failure) = compute_sons b p in
	     let best, bestscore = 
	       begin
		 match sons_failure with
		 | a::sf ->
		    let s = score b p in
		    if s >= bestscore
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
    let (best, bestscore, colored) = aux (List.rev curpath) [] min_int colored [c.p] in
    List.rev best, bestscore


		     
(** initialize the configuration by placing the pawn at the center of the top row, rounding toward the left **)
 let init (c: t): t =
   let (b, p) = (c.b, c.p) in
   let length = Board.width b in
   let (lm, rm) = horizontal_bound p (length - 1) 0 in
   let row = CellSet.fold (fun cell r -> min r cell.y) p.Pawn.cells 1 in
   if (row = 1) then failwith "Pawn not initialiazed over the first row" else
   let d = rm - lm + 1 in
   let t = (length - d) / 2 - lm in
   let p = if t > 0
	   then iter t (fun p -> Pawn.move p E) p
	   else iter (-t) (fun p -> Pawn.move p W) p in
   { b ; p }

end
