
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

		   
  
let extract_solution_from_time time =
  let fn = open_in "submissions" in
  let json = Ezjsonm.from_channel fn in
  let () = close_in fn in
  let i = parse_res json in
  match List.filter (fun {createdAt} -> createdAt = time) i with
    [] -> failwith "Invalid time for extracting"
  | {solution; seed}::_ -> solution,seed
