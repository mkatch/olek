open Core.Std
open Utils

type t = {
  pos : vector;
  w : int;
  h : int;
}

let make pos w h = {pos; w; h}

let move_by body dpos = {body with pos = body.pos +^ dpos}

let move_to body pos = {body with pos}

let to_rect {pos; w; h} = let open Vector in
  let w = Float.of_int w in
  let h = Float.of_int h in
  let l = pos.x -. 0.5 *. w in
  let t = pos.y -. 0.5 *. h
  in Rect.{
    l = l; t = t;
    r = l +. w; b = t +. h
  }

let to_sdl_rect ?offset:(offset = Vector.nil) {pos; w; h} =
  let (x, y) = Vector.to_int_pair (pos -^ offset) in
  Sdlvideo.{
    r_x = x - w / 2; r_y = y - h / 2;
    r_w = w; r_h = h
  }