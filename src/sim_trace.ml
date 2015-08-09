open Pawn
open Jfcp
open Cell
open Board
open Config
open Orders
open InOutput

(** This function executes a given path for a round and return the remaining path **)
let round_simulate rnd pawns score (board, finished, path) = 
  if finished then (board,finished, path)
  else begin
      let pawn = Prng.take rnd |> pawns in
      let preinit = { Config.b = board; Config.p = pawn } in
      let init = Config.init preinit in
      if Config.valid_config init then
	begin
	  Printf.printf ">>>>>New round!<<<<<<<\n";
	  let b = Config.proj init in
	  (* Format.printf "%a@." Pawn.format_intrep pawn; *)
	  Format.printf "%a@." Pawn.format pawn;
	  Format.printf "%a@." (Board.format ~pivot:(Some init.Config.p.Pawn.pivot)) b;
	  Printf.printf "Path = %s\n" (string_of_list_order path);
	  let (eb,path) =
	  begin match Simulation.doit init path with
		| Left (eb, restpath) -> Board.clean_end_of_round eb score (CellSet.cardinal pawn.Pawn.cells); (eb,restpath)
		| Right(cfg) -> failwith "round: path does not lead to a locked configuration"
	  end
	  in
	  eb , false, path
	end
      else (board,true, path)
    end

let play_seed_simulate (i: input_t) seed path score =
	let rnd = Prng.make seed in
	let board = Board.clone i.board in
	let (end_board, _, chemin_restant) =
	  iter i.length
	       (round_simulate rnd i.pawns score)  
	       (board,false,path)
	in
	if !verbose then Format.printf "Chemin restant: %s@." (string_of_list_order chemin_restant)

