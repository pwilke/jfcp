open Config
open Board
open Orders
open Pawn

let obind (x: 'a option) (f: 'a -> 'b option) =
  begin match x with
  | Some a -> f a
  | None -> None
  end

let doigt : Config.t option -> order list -> Config.t option =
  List.fold_left (fun cfg order ->
    obind cfg (fun cfg ->
    Format.printf "%a\n%s@."
    (Board.format ~pivot:(Some cfg.Config.p.Pawn.pivot)) (Config.proj cfg)
    (string_of_order order)
    ;
    Config.update cfg order
    )
  )

let doit cfg path =
  begin match doigt (Some cfg) path with
  | Some _ -> Printf.eprintf "Simulation: oops\n"
  | None -> ()
  end
