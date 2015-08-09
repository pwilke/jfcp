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
let rec doit (cfg: Config.t) : order list -> (Board.t * order list, Config.t) either =
  begin function
  | [] -> Right cfg
  | order :: path ->
      if ! verbose 
      then Format.printf "%a\n%s@."
           (Board.format ~pivot:(Some cfg.Config.p.Pawn.pivot))
           (Config.proj cfg)
           (string_of_order order);
      begin match Config.update cfg order with
      | Some cfg' -> doit cfg' path
      | None -> Left(Config.proj cfg, path)
      end
  end


    
exception Unsafe

let rec do_it_safe (bs: PawnSet.t) (cfg: Config.t) : order list -> (Board.t * order list, Config.t) either * PawnSet.t =
  begin function
  | [] -> Right cfg, bs
  | order :: path ->
      if ! verbose 
      then Format.printf "%a\n%s@."
           (Board.format ~pivot:(Some cfg.Config.p.Pawn.pivot))
           (Config.proj cfg)
           (string_of_order order);
      begin match Config.update cfg order with
	    | Some cfg' ->
	       let bs = (PawnSet.add cfg.Config.p bs) in
	       let pp = cfg'.Config.p in
	       if PawnSet.mem pp bs then raise Unsafe
	       else do_it_safe bs cfg' path
	    | None -> Left(Config.proj cfg, path) , bs
      end
  end


