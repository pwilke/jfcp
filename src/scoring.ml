(** Points scored through moves during a round **)
let move_score size ls ls_old =
  let points = size + 100 * (1 + ls) * ls / 2 in
  let line_bonus =
    if ls_old > 1
    then (ls_old - 1) * points / 10
    else 0 in
  points + line_bonus

(** Points scored through the invocation of a phrase of power **)
let power_score lenp repsp = 2 * lenp * repsp +
			       if repsp > 0
			       then 300 else 0
