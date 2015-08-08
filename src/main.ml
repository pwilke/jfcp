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
      let init = Config.init { Config.b = board; Config.p = pawn } in
      if Config.valid_config init then
	begin
	  let path = Config.walk init in
	  let eb = Simulation.doit init path in
	  Board.clean_end_of_round eb;
	  eb , false , curpath @ path
	end
      else (board,true,curpath)
    end

(** This function computes the series of commands for a whole game: iterates rounds over the list of available pawns **)
let play_seed (i: input_t) () seed =
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
	   tag = "";
	   solution = chemin }
	in Format.printf "%a@." pp_output out;
	   let oc = open_out_bin ("out/" ^ (string_of_int i.id)) in 
	   Ezjsonm.to_channel oc (to_jason out);
	   close_out oc
		     
(** This function plays a full game over a board per seed provided in s **)
let play_game i =  
     List.fold_left (play_seed i) 
        () i.seeds

let () =
  let filename : string list ref = ref [] in
  let timelimit : float ref = ref 0.0 in (* seconds *)
  let memolimit : float ref = ref 0.0 in (* mega-bytes *)
  let phop : string ref = ref "" in (* phrase of power *)
  let simul : bool ref = ref false in
  Arg.(parse
	 [
	   "-f", String (fun s -> filename := s :: !filename),
	   "File containing JSON encoded input.";
	   "-t", Set_float timelimit, "Time limit, in seconds, to produce output";
	   "-m", Set_float memolimit, "Memory limit, in megabytes, to produce output";
	   "-p", Set_string phop, "Phrase of power, as quoted string";
	   "-s", Set simul, "Stepwise";
	   "-v", Set verbose, "Print traces of simulation";
	 ]
	 (ignore: string -> unit)
	 "rtfm"
  );

  List.iter (fun s ->
      let fn = open_in s in
      let json = Ezjsonm.from_channel fn in
      let () = close_in fn in
      let i = parse json in
      Format.printf "%a@." pp_input i;
      play_game i)

     (!filename)


