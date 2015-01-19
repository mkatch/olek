open Core.Std
open Utils

type t = {
  surface : Sdlvideo.surface;
  offset : vector;
}

let init ~w ~h = {
  surface = Sdlvideo.set_video_mode ~w:800 ~h:600 [];
  offset = Vector.nil;
}

let dims c = let (w, h, _) = Sdlvideo.surface_dims c.surface in (w, h)

let to_world c canvas_pos = canvas_pos +^ c.offset

let to_canvas c world_pos = world_pos -^ c.offset

let center c = to_world c (0.5 *^ Vector.of_ints (dims c))

let focus center c =
  let new_offset = center -^ 0.5 *^ Vector.of_ints (dims c) in
  { c with offset = 0.05 *^ new_offset +^ 0.95 *^ c.offset }

let flip c = Sdlvideo.flip c.surface

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
  let open Sdlvideo in
  let open Room in
  let s = tileset.tile_size in
  let (ox, oy) = Vector.to_ints c.offset in
  let draw_tile i j k = if k > 0 then
    let src_rect = {
      r_x = ((k - 1) mod tileset.cols) * s; r_y = (k - 1) / tileset.cols * s;
      r_w = s; r_h = s; 
    } in
    let dst_rect = {
      r_x = j * s - ox; r_y = i * s - oy;
      r_w = s; r_h = s
    }
    in blit_surface ~src:tileset.image ~src_rect:src_rect
                    ~dst:c.surface     ~dst_rect:dst_rect () in
  Grid.iteri ~f:draw_tile grid

let draw_room_layer c = function
  | Room.Uniform color -> draw_uniform_layer c color
  | Room.Tiled (tileset, grid) -> draw_tiled_layer c tileset grid

let draw_room c room =
  List.iter ~f:(draw_room_layer c) room.Room.layers

let draw_body c body =
  let src_surface, src_rect = Sprite.blit_data (Body.sprite body) in
  let dst_rect = Body.sprite_dst_sdl_rect ~offset:c.offset body in
  let bbox_color = Sdlvideo.map_RGB c.surface Sdlvideo.red in
  let bbox = Body.bounding_box ~offset:c.offset body in
  Sdlvideo.blit_surface ~src:src_surface ~src_rect:src_rect
                        ~dst:c.surface   ~dst_rect:dst_rect ();
  put_rect c.surface bbox bbox_color