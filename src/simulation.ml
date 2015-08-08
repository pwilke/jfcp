open Jfcp
open Config
open Board
open Orders
open Pawn

(* Runs an order list from an initial configuration.
 * When an order fails,
 * the current configuration is returned in Left, with the unused orders.
 * If no order fails, the final (unlocked) configuration is returned in Right
 *)
let rec doigt (cfg: Config.t) : order list -> (Board.t * order list, Config.t) either =
  begin function
  | [] -> Right cfg
  | order :: path ->
      if ! verbose 
      then Format.printf "%a\n%s@."
           (Board.format ~pivot:(Some cfg.Config.p.Pawn.pivot))
           (Config.proj cfg)
           (string_of_order order);
      begin match Config.update cfg order with
      | Some cfg' -> doigt cfg' path
      | None -> Left(Config.proj cfg, path)
      end
  end

let doit cfg path = doigt cfg path
