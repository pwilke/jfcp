open Jfcp
open Config
open Board
open Orders
open Pawn

type ('a, 'b) either =
| Left of 'a
| Right of 'b

let ebind (x: ('a, 'b) either) (f: 'a -> ('a, 'b) either) =
  begin match x with
  | Left a -> f a
  | _ -> x
  end

let of_option (def: 'b) (x: 'a option) : ('a, 'b) either =
  begin match x with
  | Some a -> Left a
  | None -> Right def
  end

let doigt : (Config.t, Config.t) either -> order list -> (Config.t, Config.t) either =
  List.fold_left
    (fun cfg order ->
     ebind cfg (fun cfg ->
		if ! verbose 
		then Format.printf
		       "%a\n%s@."
		       (Board.format ~pivot:(Some cfg.Config.p.Pawn.pivot))
		       (Config.proj cfg)
		       (string_of_order order);
		of_option cfg (Config.update cfg order)
    )
  )

let doit cfg path =
  begin match doigt (Left cfg) path with
  | Left _ -> failwith "Simulation: oops\n"
  | Right r -> Config.proj r
  end
