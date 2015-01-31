open Utils

type t = {
  offset : int * int;
  bounds : Sdlvideo.rect;
}

let offset view = view.offset

let to_world view pos = pos -^ view.offset
let to_view view pos = pos +^ view.offset 

let focus center ?dt:(dt = 0.0) view =
  let view_center = Canvas.dims () /^ 2 in
  let lt, rb = Sdlvideo.rect_corners view.bounds in
  let center = v_clamp ~min:lt ~max:rb center in
  let offset = view_center -^ center in
  { view with offset }

let make center bounds =
  let w, h = Canvas.dims() in
  let (l, t), (r, b) = Sdlvideo.rect_corners bounds in
  let bounds = Sdlvideo.rect (l + w / 2) (t + h / 2) (r - l - w) (b - t - h) in
  focus center { offset = (0, 0); bounds }

let center view = to_world view (Canvas.dims () /^ 2)

let move_by doffset view = { view with offset = view.offset +^ doffset }

let blit view ?pos:(pos = (0, 0)) ?src_rect:(src_rect = Sdlvideo.nil_rect) src =
  Canvas.blit ~pos:(to_view view pos) ~src_rect:src_rect src

let draw_rect view color rect =
  let rect = Sdlvideo.move_rect view.offset rect in
  Canvas.draw_rect color rect

let draw_text view pos fg text = Canvas.draw_text (to_view view pos) fg text