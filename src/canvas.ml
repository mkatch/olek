open Core.Std
open Utils
open Utils.Oref

let screen = ref (Sdlvideo.create_RGB_surface [] 0 0 0 0l 0l 0l 0l)

let font = empty_oref ()

let init ~w ~h =
  let font_filename = filename_concat ["data"; "fonts"; "input.ttf"] in
  screen := Sdlvideo.set_video_mode ~w:800 ~h:600 [];
  font <?- Sdlttf.open_font font_filename 12 

let dims () = let (w, h, _) = Sdlvideo.surface_dims !screen in (w, h)

let flip () = Sdlvideo.flip !screen

let clear color =
  let open Sdlvideo in 
  let ic = map_RGB !screen color
  in fill_rect !screen ic

let screenshot () =
  let w, h = dims () in
  let surface = Sdlvideo.create_RGB_surface_format !screen [] ~w:w ~h:h in
  Sdlvideo.blit_surface ~src:!screen ~dst:surface ();
  surface

let blit ?pos:((x, y) = (0, 0)) ?src_rect:(src_rect = Sdlvideo.nil_rect) src =
  let dst_rect = Sdlvideo.rect x y 0 0 in
  if src_rect = Sdlvideo.nil_rect then
    Sdlvideo.blit_surface ~src:src
                          ~dst:!screen ~dst_rect:dst_rect ()
  else
    Sdlvideo.blit_surface ~src:src     ~src_rect:src_rect
                          ~dst:!screen ~dst_rect:dst_rect ()

let draw_horizontal_line ~y ~x_beg ~x_end color =
  let w, h = dims () in
  if between ~min:0 ~max:(h - 1) y then
  let x_beg = clamp ~min:0 ~max:(w - 1) x_beg in
  let x_end = clamp ~min:0 ~max:(w - 1) x_end in
  for x = x_beg to x_end do
    Sdlvideo.put_pixel !screen ~x:x ~y:y color
  done

let draw_vertical_line ~x ~y_beg ~y_end color =
  let w, h = dims () in
  if between ~min:0 ~max:(w - 1) x then
  let y_beg = clamp ~min:0 ~max:(h - 1) y_beg in
  let y_end = clamp ~min:0 ~max:(h - 1) y_end in
  for y = y_beg to y_end do
    Sdlvideo.put_pixel !screen ~x:x ~y:y color
  done

let draw_rect color rect =
  let (x0, y0), (x1, y1) = Sdlvideo.rect_corners rect in
  let (x1, y1) = (x1 - 1, y1 - 1) in
  let color = Sdlvideo.map_RGB !screen color in
  draw_horizontal_line y0 x0 x1 color;
  draw_horizontal_line y1 x0 x1 color;
  draw_vertical_line   x0 y0 y1 color;
  draw_vertical_line   x1 y0 y1 color

let draw_filled_rect color rect =
  let color = Sdlvideo.map_RGB !screen color in
  Sdlvideo.fill_rect ~rect:rect !screen color

let size_text text = Sdlttf.size_text !?font text

let draw_text pos fg text =
  let rendered_text = Sdlttf.render_text_blended !?font ~fg:fg text in
  blit ~pos:pos rendered_text