exception Escape of ((int * int) * (int * int)) * Jsonm.error

let json_of_src ?encoding 
    (src : [`Channel of in_channel | `String of string])
  =
  let dec d = match Jsonm.decode d with 
  | `Lexeme l -> l
  | `Error e -> raise (Escape (Jsonm.decoded_range d, e))
  | `End | `Await -> assert false
  in
  let rec value v k d = match v with 
  | `Os -> obj [] k d  | `As -> arr [] k d
  | `Null | `Bool _ | `String _ | `Float _ as v -> k v d 
  | _ -> assert false
  and arr vs k d = match dec d with 
  | `Ae -> k (`A (List.rev vs)) d
  | v -> value v (fun v -> arr (v :: vs) k) d
  and obj ms k d = match dec d with 
  | `Oe -> k (`O (List.rev ms)) d
  | `Name n -> value (dec d) (fun v -> obj ((n, v) :: ms) k) d
  | _ -> assert false
  in
  let d = Jsonm.decoder ?encoding src in
  try `JSON (value (dec d) (fun v _ -> v) d) with 
  | Escape (r, e) -> `Error (r, e)

let json_to_dst ~minify 
    (dst : [`Channel of out_channel | `Buffer of Buffer.t ]) 
    (json : _)
  =
  let enc e l = ignore (Jsonm.encode e (`Lexeme l)) in
  let rec value v k e = match v with 
  | `A vs -> arr vs k e 
  | `O ms -> obj ms k e 
  | `Null | `Bool _ | `Float _ | `String _ as v -> enc e v; k e
  and arr vs k e = enc e `As; arr_vs vs k e
  and arr_vs vs k e = match vs with 
  | v :: vs' -> value v (arr_vs vs' k) e 
  | [] -> enc e `Ae; k e
  and obj ms k e = enc e `Os; obj_ms ms k e
  and obj_ms ms k e = match ms with 
  | (n, v) :: ms -> enc e (`Name n); value v (obj_ms ms k) e
  | [] -> enc e `Oe; k e
  in
  let e = Jsonm.encoder ~minify dst in
  let finish e = ignore (Jsonm.encode e `End) in
  match json with `A _ | `O _ as json -> value json finish e
  | _ -> invalid_arg "invalid json text"

let () =
  let filename : string list ref = ref [] in
  let timelimit : float ref = ref 0.0 in (* seconds *)
  let memolimit : float ref = ref 0.0 in (* mega-bytes *)
  let phop : string ref = ref "" in (* phrase of power *)
  Arg.(parse
  [
    "-f", String (fun s -> filename := s :: !filename),
       "File containing JSON encoded input.";
    "-t", Set_float timelimit, "Time limit, in seconds, to produce output";
    "-m", Set_float memolimit, "Memory limit, in megabytes, to produce output";
    "-p", Set_string phop, "Phrase of power, as quoted string";
  ]
  (ignore: string -> unit)
  "rtfm"
  );
  List.iter (fun s ->
    let fn = open_in s in
    json_of_src (`Channel fn) |> function
    | `Error _ -> Printf.eprintf "oops\n"
    | `JSON j ->
      json_to_dst ~minify:true (`Channel stdout) j
    ;
    close_in fn
  ) (!filename)
