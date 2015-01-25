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