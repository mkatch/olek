type t = {
  x : float;
  y : float
}

let make x y = {x; y}

let of_pair (x, y) = {x; y}

let to_pair {x; y} = (x, y)

let nil = {x = 0.; y = 0.}

let ( + ) {x = x1; y = y1} {x = x2; y = y2} = {x = x1 +. x2; y = y1 +. y2}

let ( - ) {x = x1; y = y1} {x = x2; y = y2} = {x = x1 -. x2; y = y1 -. y2}

let ( * ) a {x; y} = {x = a *. x; y = a *. y}