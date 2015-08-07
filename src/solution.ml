open Orders

let trivial_symb_of_order (o: order) =
  match o with
  | M E -> "e"
  | M W -> "!"
  | M SE -> "l"
  | M SW -> "i"
  | R CW -> "d"
  | R CCW -> "k"

let trivial_string_of_order_list (ol: order list) =
  String.concat "" (List.map trivial_symb_of_order ol)
