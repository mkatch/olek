open Core.Std
open Utils

type layer =
  | Uniform of Sdlvideo.color
  | Tiled of int Grid.t
with sexp

type t = {
  tiles : Tile.t Grid.t;
  layers : layer list;
  tileset : Tileset.t;
}
with sexp

let tiles room = room.tiles

let layers room = room.layers

let tileset room = room.tileset

let layer_cnt room = List.length room.layers

let row_cnt room = Grid.row_cnt room.tiles

let column_cnt room = Grid.column_cnt room.tiles

let dims room = Grid.dims room.tiles

let layer_is_tiled room i =
  if i = (-1) then true
  else match List.nth room.layers i with
    | Some (Tiled _) -> true
    | _ -> false

let make rows cols =
  {
    tiles = Grid.make Tile.Void rows cols;
    layers = [];
    tileset = Tileset.load "dummy";
  }

let add_layer layer ?i:(i = 1) room =
  { room with layers = list_insert layer (i - 1) room.layers }

let add_uniform_layer color room =
  { room with layers = Uniform color :: room.layers }

let add_tiled_layer room =
  let r, c = dims room in
  { room with layers = Tiled (Grid.make (-1) r c) :: room.layers }

let move_layer ~src ~dst room =
  match List.nth room.layers src with
  | None -> room
  | Some layer ->
    let layers = room.layers
      |> list_rem src
      |> list_insert layer dst in
    { room with layers }  

let set_tileset name room = { room with tileset = Tileset.load name }

let put_tile i j ~layer ~tile room =
  let rec aux n layers = match n, layers with
    | _, [] -> failwith "Room.put_tile: No such layer"
    | 0, Tiled g :: layers -> Tiled (Grid.set i j tile g) :: layers
    | 0, _ -> failwith "Room.put_tile: Cannot put tile to non-tiled layer"
    | n, layer :: layers -> layer :: aux (n - 1) layers in
  if layer >= 0 then { room with layers = aux layer room.layers }
  else { room with tiles = Grid.set i j (Tile.of_int tile) room.tiles }

let draw_uniform_layer color = Canvas.clear color

let draw_tiled_layer grid tileset view =
  let (ox, oy) = View.int_offset view in 
  let src = Tileset.surface tileset in
  let s = Tile.size in
  let draw_tile i j k = if k >= 0 then
    let src_rect = Tileset.tile_rect tileset k in
    Canvas.blit ~x:(j * s - ox) ~y:(i * s - oy) ~src_rect:src_rect src in
  Grid.iteri ~f:draw_tile grid

let draw_tiles grid view =
  let (ox, oy) = View.int_offset view in
  let src = Tileset.surface Tileset.tiles in
  let s = Tile.size in
  let draw_tile i j tile = if tile <> Tile.Void then
    let k = Tile.to_int tile in
    let src_rect = Tileset.tile_rect Tileset.tiles k in
    Canvas.blit ~x:(j * s - ox) ~y:(i * s - oy) ~src_rect:src_rect src in
  Grid.iteri ~f:draw_tile grid

let draw_layer layer tileset view  = match layer with
  | Uniform color -> draw_uniform_layer color
  | Tiled grid -> draw_tiled_layer grid tileset view

let draw room ?draw_invisible:(draw_invisible = false) view  =
  List.iter ~f:(fun layer -> draw_layer layer room.tileset view) room.layers;
  if draw_invisible then draw_tiles room.tiles view