val doit :
  Config.Config.t ->
  Orders.order list ->
  (Board.Board.t * Orders.order list, Config.Config.t) Jfcp.either
