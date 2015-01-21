open Core.Std
open Utils
open Utils.Oref

let screen = ref (Sdlvideo.create_RGB_surface [] 0 0 0 0l 0l 0l 0l)

let font = empty_oref ()

let nil_rect = Sdlvideo.rect (-123) (-456) (-789) (-101112)

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

let blit ?x:(x = 0) ?y:(y = 0) ?src_rect:(src_rect = nil_rect) src =
  let dst_rect = Sdlvideo.rect x y 0 0 in
  if src_rect = nil_rect then
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

let draw_rect rect color =
  let x0, y0, x1, y1 = Rect.int_coords rect in
  draw_horizontal_line y0 x0 x1 color;
  draw_horizontal_line y1 x0 x1 color;
  draw_vertical_line   x0 y0 y1 color;
  draw_vertical_line   x1 y0 y1 color

let draw_filled_rect rect color =
  let color = Sdlvideo.map_RGB !screen color in
  Sdlvideo.fill_rect ~rect:rect !screen color

let draw_text x y ?fg:(fg = Sdlvideo.black) ?bg:(bg = Sdlvideo.white) text =
  let rendered_text = Sdlttf.render_text_shaded !?font ~fg:fg ~bg:bg text in
  blit ~x:x ~y:y rendered_text

let draw_uniform_layer color = clear color

let draw_tiled_layer view tileset grid =
  let (ox, oy) = View.int_offset view in 
  let src = Room.surface tileset in
  let s = Tile.size in
  let draw_tile i j k = if k >= 0 then
    let src_rect = Room.tileset_src_rect tileset k in
    blit ~x:(j * s - ox) ~y:(i * s - oy) ~src_rect:src_rect src in
  Grid.iteri ~f:draw_tile grid

let draw_room_layer view tileset = function
  | Room.Uniform color -> draw_uniform_layer color
  | Room.Tiled grid -> draw_tiled_layer view tileset grid

let draw_room view room =
  List.iter ~f:(draw_room_layer view (Room.tileset room)) (Room.layers room)

let draw_body view body =
  let offset = View.offset view in
  let src_surface, src_rect = Sprite.blit_data (Body.sprite body) in
  let dst_rect = Body.sprite_dst_sdl_rect ~offset:offset body in
  let bbox_color = Sdlvideo.map_RGB !screen Sdlvideo.red in
  let bbox = Body.bounding_box ~offset:offset body in
  Sdlvideo.blit_surface ~src:src_surface ~src_rect:src_rect
                        ~dst:!screen     ~dst_rect:dst_rect ();
  draw_rect bbox bbox_color
(*
let draw_terminal c terminal =
  let pos = Terminal.pos terminal in
  let text = Terminal.text terminal in
  let length = String.length text in
  let text = String.prefix text pos ^ "|" ^ String.suffix text (length - pos) in
  let rendered_text = Sdlttf.render_text_shaded c.font ~fg:Sdlvideo.white
                      ~bg:Sdlvideo.black text in
  Sdlvideo.blit_surface ~src:rendered_text ~dst:c.surface ()
*)
