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
  ]
  (ignore: string -> unit)
  "rtfm"
  );
  List.iter (fun s ->
    let fn = open_in s in
    let json = Ezjsonm.from_channel fn in
    let () = close_in fn in
    let i = parse json in
    Format.printf "%a@." pp_input i
    ;
    if ! simul then
    List.fold_left (fun () seed ->
    let rnd = Prng.make seed in
    let board = Board.clone i.board in
    let end_board =
    iter i.length (fun board ->
      let pawn = Prng.take rnd |> i.pawns in
      let init = Config.init { Config.b = board; Config.p = pawn } in
      let path = Config.walk init in
      Simulation.doit init path
    ) board
    in ()
    ) () i.seeds
    else
      ()
  ) (!filename)
