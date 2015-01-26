open Utils

type t = {
  offset : int * int;
}

let offset view = view.offset

let to_world view pos = pos -^ view.offset
let to_view view pos = pos +^ view.offset 

let focus center ?dt:(dt = 0.0) view =
  let view_center = Canvas.dims () /^ 2 in
  let offset = view_center -^ center in
  { offset }

let make center = focus center { offset = (0, 0) }

let center view = to_world view (Canvas.dims () /^ 2)

let move_by doffset view = { offset = view.offset +^ doffset }

let blit view ?pos:(pos = (0, 0)) ?src_rect:(src_rect = Sdlvideo.nil_rect) src =
  Canvas.blit ~pos:(to_view view pos) ~src_rect:src_rect src

let draw_rect view color rect =
  let rect = Sdlvideo.move_rect view.offset rect in
  Canvas.draw_rect color rect

let draw_text view pos fg text = Canvas.draw_text (to_view view pos) fg text