open Core.Std
open Utils

type t = {
  pos : vector;
  w : int;
  h : int;
  sprite : Sprite.t;
}

let make pos w h = {
  pos; w; h;
  sprite = Sprite.dummy;
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

let set_sprite sheet body = { body with sprite = Sprite.make sheet; }

let advance_sprite t body = { body with sprite = Sprite.advance t body.sprite }

let bounding_box ?offset:(offset = Vector.nil) body =
  let w = Float.of_int body.w in
  let h = Float.of_int body.h in
  let l, t = Vector.to_floats (body.pos -^ offset) in
  Rect.make l t (l +. w) (t +. h)

let sdl_rect ?offset:(offset = Vector.nil) body =
  let (x, y) = Vector.to_ints (body.pos -^ offset) in
  Sdlvideo.rect ~x:(x - body.w / 2) ~y:(y - body.h / 2)
                ~w:body.w           ~h:body.h

let sprite_dst body =
  let (x, y) = Vector.to_ints body.pos in
  let (ox, oy) = Sprite.origin (Sprite.sheet body.sprite) in
  (x - ox, y - oy)

let draw body ?draw_bbox:(draw_bbox = false) view =
  let src, src_rect = Sprite.blit_data body.sprite in
  let (x, y) = sprite_dst body in
  View.blit view ~x:x ~y:y ~src_rect:src_rect src;
  if draw_bbox then
    let bbox = Rect.to_sdl_rect (bounding_box body) in
    View.draw_rect view bbox Sdlvideo.red