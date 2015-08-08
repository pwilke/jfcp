(* Stateful PRNG *)
type t = int32 ref

(* New PRNG from a seed *)
let make (s: int) : t = ref Int32.(of_int s)

let value_of (i: int32) : int =
  Int32.(
    shift_right i 16 |>
    logand 0x7FFFl |>
    to_int
  )

(* Current value (derived from current seed) *)
let get (p: t) : int =
  value_of !p

(* Update the seed and returns the next value *)
let next (p: t) : int =
  let v = !p in
  p := Int32.( mul v 1103515245l |> add 12345l );
  get p

(* Update the seed and returns the previous value *)
let take (p: t) : int =
  let v = get p in
  let _ = next p in
  v

(* test
let () =
  let f = Printf.printf "%d\n" in
  let p = make 17 in
  f (get p);
  for i = 1 to 42 do
    f (next p)
  done
*)
