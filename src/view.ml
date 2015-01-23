open Utils

type t = {
  offset : vector;
}


let offset view = view.offset

let int_offset view = Vector.to_ints view.offset

let to_world view (x, y) = let (ox, oy) = int_offset view in (x - ox, y - oy)

let to_view view (x, y) = let (ox, oy) = int_offset view in (x + ox, y + oy)  

let focus center ?dt:(dt = 0.0) view =
  let w, h = Canvas.dims () in
  let view_center = Vector.of_ints (w / 2, h / 2) in
  let offset = view_center -^ center in
  if dt > 0.0 then
    { offset = 0.05 *^ offset +^ 0.95 *^ view.offset}
  else
    { offset }

let make center = focus center { offset = Vector.nil }

let move_by doffset view = { offset = view.offset +^ Vector.of_ints doffset }

let blit view ?x:(x = 0) ?y:(y = 0) ?src_rect:(src_rect = Sdlvideo.nil_rect)
         src =
  let (x, y) = to_view view (x, y) in
  Canvas.blit ~x:x ~y:y ~src_rect:src_rect src

let draw_rect view rect color =
  Canvas.draw_rect (Sdlvideo.move_rect (int_offset view) rect) color