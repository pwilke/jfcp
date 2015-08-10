open Jfcp
open Cell
open Pawn
open Board
open Config
open Orders
open Play
open Sim_trace
open InOutput

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


  
let () =
let filename : string list ref = ref [] in
let timelimit : float ref = ref 0.0 in (* seconds *)
let memolimit : float ref = ref 0.0 in (* mega-bytes *)
let phop : string ref = ref "" in (* phrase of power *)
let simul : bool ref = ref false in
let seed : int ref = ref 0 in
let chemin : string ref = ref "" in
let score = ref (0, 0) in
let viz = ref false in
let show_sub = ref false in
let time = ref "" in
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
	"-V", Set viz, "Just print the initial board";
	"-time", Set_string time, "Time of the solution to simulation";
	"-submitted", Set show_sub, "Print submitted scores";
	]
	(ignore: string -> unit)
	"Prière de bien vouloir vous référer à la documentation associée. Merci."
);

if !show_sub then begin
    Parse_result.show_submitted_scores ()
    ;exit 0
  end;

List.iter (fun s ->

    let fn = open_in s in
    let json = Ezjsonm.from_channel fn in
    let () = close_in fn in
    let i = parse json in
    if !verbose then Format.printf "%a@." (pp_input true) i;
    if !viz then begin Format.printf "%a@." (pp_input false) i; exit 0 end;

    if ! simul then
      begin

	let (c,seed) = if !chemin = ""
		then Parse_result.extract_solution_from_time !time
		else !chemin, !seed in 
	Format.printf "Simulating trace %s@." c ;
      Sim_trace.play_seed_simulate i seed (Solution.order_list_of_string c) score
    end
    else
      begin let jas = Play.play_game i score in
	    let oc = open_out_bin ("out/" ^ (string_of_int i.id)
				   (* ^ "_" ^ (string_of_int (fst !score / List.length i.seeds)) *)
				  ) in 
	    Ezjsonm.to_channel oc jas;
	    close_out oc;
	    Format.printf "Expected score for problem %d: %d@." i.id (fst !score / List.length i.seeds)
      end)

     (!filename)
