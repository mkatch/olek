open Core.Std

type vector = {
  x : float;
  y : float
}
with sexp

let make x y = {x; y}

let of_floats (x, y) = {x; y}

let of_ints (x, y) = {x = Float.of_int x; y = Float.of_int y}

let to_floats {x; y} = (x, y)

let to_ints {x; y} = (Float.to_int x, Float.to_int y)

let nil = {x = 0.; y = 0.}

let ( +^ ) {x = x1; y = y1} {x = x2; y = y2} = {x = x1 +. x2; y = y1 +. y2}

let ( -^ ) {x = x1; y = y1} {x = x2; y = y2} = {x = x1 -. x2; y = y1 -. y2}

let ( *^ ) a {x; y} = {x = a *. x; y = a *. y}