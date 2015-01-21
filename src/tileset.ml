open Core.Std
open Utils

type t = {
  surface : Sdlvideo.surface;
  name : string
}

let column_cnt = 8
let row_cnt = 10

let load name =
  let filename = filename_concat ["data"; "tilesets"; name ^ ".png"] in
  try
    let surface = Sdlloader.load_image filename in
    { surface; name; }
  with Sdlloader.SDLloader_exception e -> failwith ("Tileset.load: " ^ e)

let sexp_of_t tileset = sexp_of_string tileset.name 

let t_of_sexp sexp = load (string_of_sexp sexp)

let surface tileset = tileset.surface

let tile_rect tileset k =
  let i = k / column_cnt in
  let j = k mod column_cnt in
  Sdlvideo.rect ~x:(j * Tile.size) ~y:(i * Tile.size) ~w:Tile.size ~h:Tile.size

let src_rect = Sdlvideo.rect 0 0 (column_cnt * Tile.size) (row_cnt * Tile.size)
let draw tileset ~x ~y ~active =
  let active_rect = tile_rect tileset active in
  let active_outer_rect = Sdlvideo.inflate_rect   1  active_rect in
  let active_inner_rect = Sdlvideo.inflate_rect (-1) active_rect in
  Canvas.blit ~x:x ~y:y ~src_rect:src_rect tileset.surface;
  Canvas.draw_rect active_outer_rect Sdlvideo.red;
  Canvas.draw_rect active_inner_rect Sdlvideo.red