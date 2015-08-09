
(* {"powerScore":0,"seed":0,"tag":"","createdAt":"2015-08-08T13:14:25.752Z","score":0,"authorId":479,"teamId":175,"problemId":0,"solution":""} *)

(* problem_id; seed; createdAt; score; powerScore *)

type sub_t = { pid: int; seed: int; createdAt: string; score: int; powerScore: int }

let parse_res json : sub_t list =
  let open Ezjsonm in

  let parse_elt json : sub_t =
    let pid = get_int (find json ["problemId"]) in
    let seed = get_int (find json ["seed"]) in
    let score = get_int (find json ["score"]) in
    let createdAt = get_string (find json ["createdAt"]) in
    let powerScore = get_int (find json ["powerScore"]) in
    { pid; seed; createdAt; score; powerScore }
  in
  json |> get_list parse_elt


let show_submitted_scores () =
  let fn = open_in "submissions" in
  let json = Ezjsonm.from_channel fn in
  let () = close_in fn in
  let i = parse_res json in
  let i = List.sort (fun i1 i2 ->
		     compare i1.createdAt i2.createdAt
		    ) i in
  List.iter (fun {pid;seed;createdAt;score;powerScore} ->
	     Format.printf "Problem %d / Seed %d / Created at %s : %d@." pid seed createdAt score
	    ) i

		   
  
  (* let f k = find json k in *)
  
  
  
  (* let get_cell c = *)
  (*   let f k = get_int (find c k) in *)
  (*   { x = f ["x"]; y = f ["y"] } *)
  (* in *)
  (* let get_pawn p = *)
  (*   { *)
  (*     Pawn.cells = find p ["members"] |> get_list get_cell |> cellset_of_list *)
  (*   ; *)
  (*     Pawn.pivot = get_cell (find p ["pivot"]) *)
  (*   } *)
  (* in *)

  (* let width = get_int (f ["width"]) in *)
  (* let height = get_int (f ["height"]) in *)
  (* let filled = f ["filled"] |> get_list get_cell in *)
  (* let pawns = f ["units"] |> get_list get_pawn |> Array.of_list in *)
  (* let size = Array.length pawns in *)
  (* let pawns = fun b -> pawns.(b mod size) in *)
  (* { *)
  (*   id = get_int (f ["id"]); *)
  (*   width; *)
  (*   height; *)
  (*   board = Board.init height width filled; *)
  (*   length = get_int (f ["sourceLength"]); *)
  (*   seeds = get_list get_int (f ["sourceSeeds"]); *)
  (*   size; pawns; *)
  (* } *)
