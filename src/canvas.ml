open Core.Std
open Utils

type t = {
  surface : Sdlvideo.surface;
  offset : vector;
  font : Sdlttf.font;
}

let init ~w ~h =
  let font_filename = filename_concat ["data"; "fonts"; "input.ttf"] in
  {
    surface = Sdlvideo.set_video_mode ~w:800 ~h:600 [];
    offset = Vector.nil;
    font = Sdlttf.open_font font_filename 14;
  }

let set_font c font = { c with font }

let dims c = let (w, h, _) = Sdlvideo.surface_dims c.surface in (w, h)

let to_world c canvas_pos = canvas_pos +^ c.offset

let to_canvas c world_pos = world_pos -^ c.offset

let center c = to_world c (0.5 *^ Vector.of_ints (dims c))

let focus center c =
  let new_offset = center -^ 0.5 *^ Vector.of_ints (dims c) in
  { c with offset = 0.05 *^ new_offset +^ 0.95 *^ c.offset }

let flip c = Sdlvideo.flip c.surface

let clear c color =
  let open Sdlvideo in 
  let ic = map_RGB c.surface color
  in fill_rect c.surface ic

let put_horizontal_line surface ~y ~x_beg ~x_end color =
  let w, h, _ = Sdlvideo.surface_dims surface in
  if between ~min:0 ~max:(h - 1) y then
  let x_beg = clamp ~min:0 ~max:(w - 1) x_beg in
  let x_end = clamp ~min:0 ~max:(w - 1) x_end in
  for x = x_beg to x_end do
    Sdlvideo.put_pixel surface ~x:x ~y:y color
  done

let put_vertical_line surface ~x ~y_beg ~y_end color =
  let w, h, _ = Sdlvideo.surface_dims surface in
  if between ~min:0 ~max:(w - 1) x then
  let y_beg = clamp ~min:0 ~max:(h - 1) y_beg in
  let y_end = clamp ~min:0 ~max:(h - 1) y_end in
  for y = y_beg to y_end do
    Sdlvideo.put_pixel surface ~x:x ~y:y color
  done

let put_rect surface rect color =
  let x0, y0, x1, y1 = Rect.int_coords rect in
  put_horizontal_line surface y0 x0 x1 color;
  put_horizontal_line surface y1 x0 x1 color;
  put_vertical_line   surface x0 y0 y1 color;
  put_vertical_line   surface x1 y0 y1 color

let draw_uniform_layer c color =
  let open Sdlvideo in
  let ic = map_RGB c.surface color
  in fill_rect c.surface ic

let draw_tiled_layer c tileset grid =
  let src_surface = Room.surface tileset in
  let s = Tile.size in
  let (ox, oy) = Vector.to_ints c.offset in
  let draw_tile i j k = if k >= 0 then
    let src_rect = Room.tileset_src_rect tileset k in
    let dst_rect = Sdlvideo.rect ~x:(j * s - ox) ~y:(i * s - oy) ~w:0 ~h:0 in
    Sdlvideo.blit_surface ~src:src_surface ~src_rect:src_rect
                          ~dst:c.surface   ~dst_rect:dst_rect () in
  Grid.iteri ~f:draw_tile grid

let draw_room_layer c tileset = function
  | Room.Uniform (r, g, b) -> draw_uniform_layer c (r, g, b)
  | Room.Tiled grid -> draw_tiled_layer c tileset grid

let draw_room c room =
  List.iter ~f:(draw_room_layer c (Room.tileset room)) (Room.layers room)

let draw_body c body =
  let src_surface, src_rect = Sprite.blit_data (Body.sprite body) in
  let dst_rect = Body.sprite_dst_sdl_rect ~offset:c.offset body in
  let bbox_color = Sdlvideo.map_RGB c.surface Sdlvideo.red in
  let bbox = Body.bounding_box ~offset:c.offset body in
  Sdlvideo.blit_surface ~src:src_surface ~src_rect:src_rect
                        ~dst:c.surface   ~dst_rect:dst_rect ();
  put_rect c.surface bbox bbox_color

let draw_terminal c terminal =
  let pos = Terminal.pos terminal in
  let text = Terminal.text terminal in
  let length = String.length text in
  let text = String.prefix text pos ^ "|" ^ String.suffix text (length - pos) in
  let rendered_text = Sdlttf.render_text_shaded c.font ~fg:Sdlvideo.white
                      ~bg:Sdlvideo.black text in
  Sdlvideo.blit_surface ~src:rendered_text ~dst:c.surface ()
