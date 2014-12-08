type t = {
  l : float;
  t : float;
  r : float;
  b : float;
}

let make l t r b = {l; t; r; b}

let width rect = rect.r -. rect.l

let height rect = rect.b -. rect.t

(*
let to_sdl_rect {l; t; r; b} = Sdlvideo.{
  r_x = Float.to_int l; r_y = Float.to_int t;
  r_w = Float.to_int (r -. l); r_h = Float.to_int (b -. t)
} *)