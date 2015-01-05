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

let center c = to_world c (0.5 *^ Vector.of_int_pair (dims c))

let focus center c =
  let new_offset = Vector.(center -^  0.5 *^ of_int_pair (dims c)) in
  { c with offset = new_offset }

let flip c = Sdlvideo.flip c.surface

let draw_uniform_layer c color =
  let open Sdlvideo in
  let ic = map_RGB c.surface color
  in fill_rect c.surface ic

let draw_tiled_layer c tileset grid =
  let open Sdlvideo in
  let open Room in
  let s = tileset.tile_size in
  let (ox, oy) = Vector.to_int_pair c.offset in
  let draw_tile i j k =
    let rect = {
      r_x = i * s - ox; r_y = j * s - oy;
      r_w = s; r_h = s
    }
    in blit_surface ~src:tileset.image ~src_rect:rect
                    ~dst:c.surface     ~dst_rect:rect () in
  Grid.iteri ~f:draw_tile ~r_beg:0 ~c_beg:0 ~r_end:5 ~c_end:5 grid

let draw_room_layer c = function
  | Room.Uniform color -> draw_uniform_layer c color
  | Room.Tiled (tileset, grid) -> draw_tiled_layer c tileset grid

let draw_room c room =
  List.iter ~f:(draw_room_layer c) room.Room.layers

let draw_body c body =
  let color = Sdlvideo.(map_RGB c.surface black) in
  let r = Body.to_sdl_rect ~offset:c.offset body in
  Sdlvideo.fill_rect ~rect:r c.surface color