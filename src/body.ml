open Core.Std
open Utils

type t = {
  pos : vector;
  w : int;
  h : int;
  sprite : Sprite.instance;
  sprite_offset : vector;
}

let make pos w h = {
  pos; w; h;
  sprite = Sprite.dummy_instance;
  sprite_offset = Vector.nil
}

let w body = body.w

let set_w w body = { body with w }

let h body = body.h

let set_h h body = { body with h }

let dims body = (body.w, body.h)

let set_dims w h body = { body with w; h } 

let pos body = body.pos

let set_pos pos body = { body with pos }

let move_by dpos body = { body with pos = body.pos +^ dpos }

let sprite body = body.sprite

let set_sprite sheet ?offset:(offset = Vector.nil) body =
  { body with
    sprite = Sprite.make_instance sheet;
    sprite_offset = offset }

let advance_sprite t body = { body with sprite = Sprite.advance t body.sprite }
(*
let rect {pos; w; h} = let open Vector in
  let w = Float.of_int w in
  let h = Float.of_int h in
  let l = pos.x -. 0.5 *. w in
  let t = pos.y -. 0.5 *. h
  in Rect.{
    l = l; t = t;
    r = l +. w; b = t +. h
  }
*)
let sdl_rect ?offset:(offset = Vector.nil) body =
  let (x, y) = Vector.to_ints (body.pos -^ offset) in
  Sdlvideo.rect ~x:(x - body.w / 2) ~y:(y - body.h / 2)
                ~w:body.w           ~h:body.h

let sprite_dst_sdl_rect ?offset:(offset = Vector.nil) body =
  let x, y = Vector.to_ints (body.pos -^ offset +^ body.sprite_offset) in
  let x = x - body.w / 2 and y = y - body.h / 2 in
  Sdlvideo.rect x y 0 0 (* Width and height are unimportant *)