open Core.Std

type t = {
  l : float;
  t : float;
  r : float;
  b : float;
}

let make l t r b = {l; t; r; b}

let l rect = rect.l

let t rect = rect.t

let r rect = rect.r

let b rect = rect.b

let w rect = rect.r -. rect.l

let h rect = rect.b -. rect.t

let coords { l; t; r; b; } = (l, t, r, b)

let int_coords { l; t; r; b; } = (
  Float.to_int l,
  Float.to_int t,
  Float.to_int r,
  Float.to_int b
)

let to_sdl_rect {l; t; r; b} =
  Sdlvideo.rect ~x:(Float.to_int l)        ~y:(Float.to_int t)
                ~w:(Float.to_int (r -. l)) ~h:(Float.to_int (b -. t))