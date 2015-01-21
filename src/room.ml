open Core.Std
open Utils

type tileset = {
  surface : Sdlvideo.surface;
  rows : int;
  cols : int;
  name : string;
}


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

let make rows cols =
  {
    tiles = Grid.make Tile.Void rows cols;
    layers = [];
    tileset = Tileset.load "spring";
  }

let tiles room = room.tiles

let layers room = room.layers

let tileset room = room.tileset

let add_layer layer ?i:(i = 1) room =
  { room with layers = list_insert layer (i - 1) room.layers }

let move_layer ~src ~dst room =
  match List.nth room.layers src with
  | None -> room
  | Some layer ->
    let layers = room.layers
      |> list_rem src
      |> list_insert layer dst in
    { room with layers }  

let set_tileset name room = { room with tileset = Tileset.load name }

let input_tiles inch =
  let rec aux rrows =
    let line = Option.value_exn (In_channel.input_line inch) in
    if String.is_empty line then List.rev rrows
    else
      let row = List.map ~f:(fun _ -> Tile.Void) (String.to_list line) in
      aux (row :: rrows) in
  let rows = aux [] in
  Printf.printf "rows: %d\n" (List.length rows);
  Grid.of_lists rows

let load name =
  let filename = filename_concat ["data"; "rooms"; name ^ ".orm"] in
  let file = In_channel.create filename in
  let tiles = input_tiles file in
  let layers = [] in
  { tiles; layers; tileset = (Tileset.load "dummy") }

let make_tiles_layer tiles =
  let aux = function
    | Tile.Void -> 0
    | Tile.Solid -> 2
    | Tile.TopSolid -> 4
    | Tile.Sticky -> 5 in
  let grid = Grid.map ~f:aux tiles in
  Tiled grid

let add_tiles_layer room =
  let tl = make_tiles_layer room.tiles in
  { room with layers = room.layers @ [tl] }

let draw_uniform_layer color = Canvas.clear color

let draw_tiled_layer grid tileset view =
  let (ox, oy) = View.int_offset view in 
  let src = Tileset.surface tileset in
  let s = Tile.size in
  let draw_tile i j k = if k >= 0 then
    let src_rect = Tileset.tile_rect tileset k in
    Canvas.blit ~x:(j * s - ox) ~y:(i * s - oy) ~src_rect:src_rect src in
  Grid.iteri ~f:draw_tile grid

let draw_layer layer tileset view  = match layer with
  | Uniform color -> draw_uniform_layer color
  | Tiled grid -> draw_tiled_layer grid tileset view

let draw room view =
  List.iter ~f:(fun layer -> draw_layer layer room.tileset view) room.layers