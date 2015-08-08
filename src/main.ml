open Jfcp
open Cell
open Pawn
open Board
open Config
open Orders
       
(*
{ "id": number              /* A unique number identifying the problem */

, "units": [ Unit ]
  /* The various unit configurations that may appear in this game.
     There might be multiple entries for the same unit.
     When a unit is spawned, it will start off in the orientation
     specified in this field. */

, "width":  number          /* The number of cells in a row */

, "height": number          /* The number of rows on the board */

, "filled": [ Cell ]        /* Which cells start filled */

, "sourceLength": number    /* How many units in the source */

, "sourceSeeds": [ number ] /* How to generate the source and
                               how many games to play */
}
 *)
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

let parse json =
  let open Ezjsonm in
  let get_cell c =
    let f k = get_int (find c k) in
    { x = f ["x"]; y = f ["y"] }
  in
  let get_pawn p =
    {
      Pawn.cells = find p ["members"] |> get_list get_cell |> cellset_of_list
    ;
      Pawn.pivot = get_cell (find p ["pivot"])
    }
  in
  let f k = find json k in
  let width = get_int (f ["width"]) in
  let height = get_int (f ["height"]) in
  let filled = f ["filled"] |> get_list get_cell in
  let pawns = f ["units"] |> get_list get_pawn |> Array.of_list in
  let size = Array.length pawns in
  let pawns = fun b -> pawns.(b mod size) in
  {
    id = get_int (f ["id"]);
    width;
    height;
    board = Board.init height width filled;
    length = get_int (f ["sourceLength"]);
    seeds = get_list get_int (f ["sourceSeeds"]);
    size; pawns;
  }

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

(** This function computes the path for a round: given a board and a pawn about to fall, where do we lead it **)
let round rnd pawns (board, finished, curpath) = 
  if finished then (board,finished, curpath)
  else begin
      let pawn = Prng.take rnd |> pawns in
      let preinit = { Config.b = board; Config.p = pawn } in
      let init = Config.init preinit in
      if Config.valid_config init then
	begin
	  let path = Config.walk init in
	  if !verbose then
	    begin
	      let preb = Config.proj preinit in
	      let b = Config.proj init in	      
	      Printf.printf ">>>>>New round!<<<<<<<\n";
	      Format.printf "%a@." (Board.format ~pivot:(Some preinit.Config.p.Pawn.pivot)) preb;
	      Format.printf "%a@." Pawn.format_intrep pawn;
	      Format.printf "%a@." Pawn.format pawn;
	      Format.printf "%a@." (Board.format ~pivot:(Some init.Config.p.Pawn.pivot)) b;
	      Printf.printf "Path = %s\n" (string_of_list_order path);
	    end;
	  let eb =

	  begin match Simulation.doit init path with
    | Left (eb, []) -> Board.clean_end_of_round eb; eb
    | Left (eb, morepath) -> failwith "round: trailing orders at the end of path"
    | Right(cfg) -> failwith "round: path does not lead to a locked configuration"
    end
    in

	  eb , false , curpath @ path
	end
      else (board,true,curpath)
    end


(** This function executes a given path for a round and return the remaining path **)
let round_simulate rnd pawns (board, finished, path) = 
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
		| Left (eb, restpath) -> Board.clean_end_of_round eb; (eb,restpath)
		| Right(cfg) -> failwith "round: path does not lead to a locked configuration"
	  end
	  in
	  eb , false, path
	end
      else (board,true, path)
    end

	 
(** This function computes the series of commands for a whole game: iterates rounds over the list of available pawns **)

let play_seed jas (i: input_t) seed =
	let rnd = Prng.make seed in
	let board = Board.clone i.board in
	let (end_board, _, chemin) =
	  (* finished is a boolean that is set to true when a pawn cannot be placed in the grid (e.g. full grid). When it is set we stop iterating on following pawns. *)
	  iter i.length
	       (round rnd i.pawns)  
	       (board,false,[])
	in
	let out :output_t =
	  {pb_id = i.id;
	   seed = seed;
	   tag = "sunday";
	   solution = chemin }
	in if !verbose then Format.printf "%a@." pp_output out;
	   match jas with 
	   | ` A j -> `A ((to_jason out)::j)
	   | _     -> failwith "not jas" 
			       
let play_seed_simulate (i: input_t) seed path =
	let rnd = Prng.make seed in
	let board = Board.clone i.board in
	let (end_board, _, chemin_restant) =
	  iter i.length
	       (round_simulate rnd i.pawns)  
	       (board,false,path)
	in
	if !verbose then Format.printf "Chemin restant: %s@." (string_of_list_order chemin_restant)

		     
(** This function plays a full game over a board per seed provided in s **)
let play_game i =  
     List.fold_left (fun jas s -> play_seed jas i s) 
        (`A []) i.seeds

let () =
  let filename : string list ref = ref [] in
  let timelimit : float ref = ref 0.0 in (* seconds *)
  let memolimit : float ref = ref 0.0 in (* mega-bytes *)
  let phop : string ref = ref "" in (* phrase of power *)
  let simul : bool ref = ref false in
  let seed : int ref = ref 0 in
  let chemin : string ref = ref "" in
  Arg.(parse
	 [
	   "-f", String (fun s -> filename := s :: !filename),
	   "File containing JSON encoded input.";
	   "-t", Set_float timelimit, "Time limit, in seconds, to produce output";
	   "-m", Set_float memolimit, "Memory limit, in megabytes, to produce output";
	   "-p", Set_string phop, "Phrase of power, as quoted string";
	   "-s", Set simul, "Simulate a given path";
	   "-c", Set_string chemin, "Path to simulate";
	   "-r", Set_int seed, "Seed to simulate on";
	   "-v", Set verbose, "Print traces of simulation";
	 ]
	 (ignore: string -> unit)
	 "Prière de bien vouloir vous référer à la documentation associée. Merci."
  );

  List.iter (fun s ->

      let fn = open_in s in
      let json = Ezjsonm.from_channel fn in
      let () = close_in fn in
      let i = parse json in
      if !verbose then Format.printf "%a@." pp_input i;

      if ! simul then
	begin
	  play_seed_simulate i !seed (Solution.order_list_of_string !chemin)
	end
      else
	begin let jas = play_game i in
	      let oc = open_out_bin ("out/" ^ (string_of_int i.id)) in 
	      Ezjsonm.to_channel oc jas;
	      close_out oc
	end)

     (!filename)



