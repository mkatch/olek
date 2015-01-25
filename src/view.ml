open Utils

type t = {
  offset : float * float;
}

let to_world view pos = pos -^ v_to_ints view.offset
let to_view view pos = pos +^ v_to_ints view.offset  

let focus center ?dt:(dt = 0.0) view =
  let view_center = v_to_floats (Canvas.dims () /^ 2) in
  let offset = view_center -.^ center in
  if dt > 0.0 then
    { offset = 0.05 *.^ offset +.^ 0.95 *.^ view.offset }
  else
    { offset }

let make center = focus center { offset = (0., 0.) }

let move_by doffset view = { offset = view.offset +.^ v_to_floats doffset }

let blit view ?pos:(pos = (0, 0)) ?src_rect:(src_rect = Sdlvideo.nil_rect) src =
  Canvas.blit ~pos:(to_view view pos) ~src_rect:src_rect src

let draw_rect view color rect =
  let rect = Sdlvideo.move_rect (v_to_ints view.offset) rect in
  Canvas.draw_rect color rect

let draw_text view pos fg text = Canvas.draw_text (to_view view pos) fg text