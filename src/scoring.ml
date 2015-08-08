let move_score size ls ls_old =
  size + 100 * (1 + ls) * ls / 2 +
    if ls_old > 1
    then floor ((ls_old - 1) * points / 10)
    else 0

let power_score lenp repsp = 2 * lenp * repsp +
			       if repsp > 0
			       then 300 else 0
