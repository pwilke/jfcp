open Jfcp
open InOutput
(* {"powerScore":0,"seed":0,"tag":"","createdAt":"2015-08-08T13:14:25.752Z","score":0,"authorId":479,"teamId":175,"problemId":0,"solution":""} *)

(* problem_id; seed; createdAt; score; powerScore *)

type sub_t = { pid: int; seed: int; createdAt: string; score: int; powerScore: int; solution: string }

let parse_res json : sub_t list =
  let open Ezjsonm in

  let parse_elt json : sub_t =
    let pid = get_int (find json ["problemId"]) in
    let seed = get_int (find json ["seed"]) in
    let score = try get_int (find json ["score"]) with _ -> -1 in
    let createdAt = get_string (find json ["createdAt"]) in
    let solution = get_string (find json ["solution"]) in
    let powerScore = try get_int (find json ["powerScore"]) with _ -> -1 in
    { pid; seed; createdAt; score; powerScore; solution }
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
	     Format.printf "Problem %d / Seed %d / Created at %s : %d / %d wop%s@." pid seed createdAt score powerScore (if powerScore = 1 then "" else "s")
	    ) i

type out_t = {opid: int; oseed:int ; osol: string}
	    
let parse_out json : out_t list =
  let open Ezjsonm in

  let parse_elt json : out_t =
    let pid = get_int (find json ["problemId"]) in
    let seed = get_int (find json ["seed"]) in
    let solution = get_string (find json ["solution"]) in
    { opid = pid; oseed = seed; osol = solution }
  in
  json |> get_list parse_elt
    

let show_best_scores () =
  let fn = open_in "best_soum" in
  let json = Ezjsonm.from_channel fn in
  let () = close_in fn in
  let i = parse_out json in
  let i = List.sort (fun i1 i2 ->
		     compare i1.opid i2.opid
		    ) i in
  List.iter (fun {opid;oseed;osol} ->
	     Format.printf "Problem %d / Seed %d @." opid oseed
	    ) i

	    
let extract_best_solutions () =
  let fn = open_in "submissions" in
  let json = Ezjsonm.from_channel fn in
  let () = close_in fn in
  let i = parse_res json in
  let i = List.sort (fun i1 i2 -> compare i1.score i2.score) i in
  let h : ((int*int), int*output_t) Hashtbl.t = Hashtbl.create 17 in
  List.iter (fun {pid;seed;createdAt;solution; score} ->
	     let b = try let (s, _) = Hashtbl.find h (pid,seed) in
			 score > s
		     with _ -> true in
	     if b
	     then
	       Hashtbl.replace h (pid,seed) (score,
					     { pb_id = pid; seed; tag="final";
					  solution });
	    ) i;
  let best_sols = Hashtbl.fold (fun k v acc ->
				snd v :: acc
			       ) h [] in
  (* List.iter (fun sol -> *)
  (* 	     Format.printf "%a@." pp_output sol) best_sols; *)

  `A (List.map to_jason best_sols)
		   
  
let extract_solution_from_time time =
  let fn = open_in "submissions" in
  let json = Ezjsonm.from_channel fn in
  let () = close_in fn in
  let i = parse_res json in
  match List.filter (fun {createdAt} -> createdAt = time) i with
    [] -> failwith "Invalid time for extracting"
  | {solution; seed}::_ -> solution,seed
