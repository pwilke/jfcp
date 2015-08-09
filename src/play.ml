open Pawn
open Jfcp
open Cell
open Board
open Config
open Orders
open InOutput

(** This function computes the path for a round: given a board and a pawn about to fall, where do we lead it **)
let round rnd pawns score (board, finished, curpath) = 
  if finished then (board,finished, curpath)
  else begin
      let pawn = Prng.take rnd |> pawns in
      let preinit = { Config.b = board; Config.p = pawn } in
      let init = Config.init preinit in
      if Config.valid_config init then
	begin
	  let (path,bestscore) = Config.walk init PawnSet.empty in
	  let (path,bestscore) =
	    List.fold_left
	      (fun (path,bestscore) elt -> 
	       let prefix = Solution.order_list_of_string elt in
	       begin
		 try
		   match Simulation.do_it_safe (PawnSet.empty) init prefix with
		   | Left c, _ -> (path,bestscore)
		   | Right c, pset ->
		      let (path_aux,bestscore_aux) = Config.walk c pset in
		      if bestscore_aux >= bestscore
		      then (prefix@path_aux,bestscore_aux)
		      else (path,bestscore)
		 with Simulation.Unsafe -> (path,bestscore)
	       end)
	      (path,bestscore)
	      [ "ei!";"ia! ia!";"yuggoth";"cthulhu";"r'lyeh"]
	  in
	  if !verbose then
	    begin
	      let preb = Config.proj preinit in
	      let b = Config.proj init in	      
	      Printf.printf ">>>>>New round!<<<<<<<\n";
	      Format.printf "%s%d@." "Current score is: " (fst !score);
	      Format.printf "%a@." (Board.format ~pivot:(Some preinit.Config.p.Pawn.pivot)) preb;
	      Format.printf "%a@." Pawn.format_intrep pawn;
	      Format.printf "%a@." Pawn.format pawn;
	      Format.printf "%a@." (Board.format ~pivot:(Some init.Config.p.Pawn.pivot)) b;
	      Printf.printf "Path = %s\n" (string_of_list_order path);
	    end;
	  let eb =

	  begin match Simulation.doit init path with
    | Left (eb, []) -> Board.clean_end_of_round eb score (CellSet.cardinal pawn.Pawn.cells); eb
    | Left (eb, morepath) ->
        Printf.sprintf "round: trailing orders at the end of path\n%s\n%s"
        (string_of_list_order path)
        (string_of_list_order morepath)
        |> failwith
    | Right(cfg) ->
        Printf.sprintf "round: path does not lead to a locked configuration\n%s"
        (string_of_list_order path)
        |> failwith
    end
    in

	  eb , false , curpath @ path
	end
      else (board,true,curpath)
    end

(** This function computes the series of commands for a whole game: iterates rounds over the list of available pawns **)

let play_seed jas (i: input_t) seed score =
  if !verbose then Printf.printf ">>>>>New seed!<<<<<<<\n\n" else ();
  let temp = fst !score in
  let rnd = Prng.make seed in
  let board = Board.clone i.board in
  let (_, _, chemin) =
    (* finished is a boolean that is set to true when a pawn cannot be placed in the grid (e.g. full grid). When it is set we stop iterating on following pawns. *)
    iter i.length
	 (round rnd i.pawns score)  
	 (board,false,[])
  in
  if !verbose then Format.printf "%s%d@,%d@." "Seed: " seed (fst !score - temp);
  let out :output_t =
    {pb_id = i.id;
     seed = seed;
     tag = "sunday";
     solution = chemin }
  in if !verbose then Format.printf "%a@." pp_output out;
     match jas with 
     | ` A j -> `A ((to_jason out)::j)
     | _     -> failwith "not jas" 

(** This function plays a full game over a board per seed provided in s **)
let play_game i score =
  List.fold_left (fun jas s ->
		  score := (fst (! score), 0);
		  let res = play_seed jas i s score in


		  
		  (* Printf.printf "After playing seed %d, score is (%d,%d), avg is %d\n" *)
		  (* 		s (fst (! score)) (snd (! score)) *)
		  (* 		(fst (!score) / List.length (i.seeds)) *)
		  (* ;  *)
		  res) 
        (`A []) i.seeds

