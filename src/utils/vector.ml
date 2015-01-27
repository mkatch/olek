open Core.Std

let vx (x, _) = x
let vy (_, y) = y

let v_to_ints (x, y) = (Float.to_int x, Float.to_int y)
let v_to_floats (x, y) = (Int.to_float x, Int.to_float y)

let ( +^ ) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
let ( -^ ) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
let ( *^ ) a (x, y) = (a * x, a * y)
let ( /^ ) (x, y) a = (x / a, y / a)

let ( +.^ ) (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2)
let ( -.^ ) (x1, y1) (x2, y2) = (x1 -. x2, y1 -. y2)
let ( *.^ ) a (x, y) = (a *. x, a *. y)
let ( /.^ ) (x, y) a = (x /. a, y /. a)

let dot (x0, y0) (x1, y1) = x0 *. x1 +. y0 *. y1
let per (x0, y0) (x1, y1) = y0 *. x1 -. x0 *. y1

let v_clamp ~min:(x_min, y_min) ~max:(x_max, y_max) (x, y) = (
  (if x <= x_min then x_min else if x_max <= x then x_max else x),
  (if y <= y_min then y_min else if y_max <= y then y_max else y)
)