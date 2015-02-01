open Core.Std
open Utils

type t = {
  x : float;
  y : float;
  w : int;
  h : int;
  sprite : Sprite.t;
  visible : bool;
}

let make x y w h = {
  x; y; w; h;
  sprite = Sprite.dummy;
  visible = true;
}

let x body = body.x
let y body = body.y
let pos body = (body.x, body.y)
let w body = body.w
let h body = body.h
let dims body = (body.w, body.h)
let l body = body.x -. 0.5 *. Float.of_int body.w
let t body = body.y -. 0.5 *. Float.of_int body.h
let r body = body.x +. 0.5 *. Float.of_int body.w
let b body = body.y +. 0.5 *. Float.of_int body.h
let lt body = (l body, t body)
let rt body = (r body, t body)
let rb body = (r body, b body)
let lb body = (l body, b body)
let sprite body = body.sprite
let visible body = body.visible
let rect body =
  let (x, y) = v_to_ints (lt body) in
  Sdlvideo.rect x y body.w body.h

let set_x x body = { body with x }
let set_y y body = { body with y }
let set_pos (x, y) body = { body with x; y }
let move_by (dx, dy) body = { body with x = body.x +. dx; y = body.y +. dy }
let set_w w body = { body with w }
let set_h h body = { body with h }
let set_dims (w, h) body = { body with w; h }
let set_visible visible body = { body with visible }

let set_sprite sheet ?force:(force = false) body =
  if force || not (phys_equal (Sprite.sheet body.sprite) sheet)
  then { body with sprite = Sprite.make sheet }
  else body 

let advance_sprite t body = { body with sprite = Sprite.advance t body.sprite }

let intersect body1 body2 =
  l body2 <= r body1 && l body1 <= r body2 &&
  t body2 <= b body1 && t body1 <= b body2

let draw view body = if body.visible then
  Sprite.draw view (v_to_ints (pos body)) body.sprite