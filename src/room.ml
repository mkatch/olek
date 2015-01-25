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
  stubs : Object.stub list;
}
with sexp

let make row_cnt column_cnt = {
  tiles = Grid.make Tile.Void row_cnt column_cnt;
  layers = [];
  tileset = Tileset.load "dummy";
  stubs = [];
}

let tiles room = room.tiles
let layers room = room.layers
let tileset room = room.tileset
let stubs room = room.stubs
let layer_cnt room = List.length room.layers
let row_cnt room = Grid.row_cnt room.tiles
let column_cnt room = Grid.column_cnt room.tiles
let dims room = Grid.dims room.tiles
let dims_px room = Tile.size *^ dims room

let layer_is_tiled room i =
  if i = (-1) then true
  else match List.nth room.layers i with
    | Some (Tiled _) -> true
    | _ -> false

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

let add_stub stub room = { room with stubs = stub :: room.stubs }

let select_stub pos room =
  let rec aux stubs accum =
    match stubs with
    | [] -> (room.stubs, false)
    | stub :: stubs ->
      if Object.stub_contains pos stub then
        (stub :: List.rev_append accum stubs, true)
      else aux stubs (stub :: accum) in
  let stubs, found  = aux room.stubs [] in
  ({ room with stubs }, found)

let map_selected_stub ~f room =
  match room.stubs with
  | [] -> room
  | stub :: stubs -> { room with stubs = f stub :: stubs }

let draw_uniform_layer color = Canvas.clear color

let draw_tiled_layer grid tileset view =
  let src = Tileset.surface tileset in
  let s = Tile.size in
  let draw_tile i j k = if k >= 0 then
    let src_rect = Tileset.tile_rect tileset k in
    View.blit view ~pos:(j * s, i * s) ~src_rect:src_rect src in
  Grid.iteri ~f:draw_tile grid

let draw_layer view tileset layer =
  match layer with
  | Uniform color -> draw_uniform_layer color
  | Tiled grid -> draw_tiled_layer grid tileset view

let draw_tiles view grid =
  let src = Tileset.surface Tileset.tiles in
  let s = Tile.size in
  let draw_tile i j tile = if tile <> Tile.Void then
    let k = Tile.to_int tile in
    let src_rect = Tileset.tile_rect Tileset.tiles k in
    View.blit view ~pos:(j * s, i * s) ~src_rect:src_rect src in
  Grid.iteri ~f:draw_tile grid

let draw_frame view room =
  let w, h = dims_px room in
  let frame = Sdlvideo.rect 0 0 w h |> Sdlvideo.inflate_rect 2 in
  View.draw_rect view Sdlvideo.black frame

let draw_stubs view ?draw_frames:(draw_frames = false) stubs =
  let aux = Object.draw_stub view ~draw_frame:draw_frames in
  match stubs with
  | [] -> ()
  | stub :: stubs ->
    aux ~frame_color:Sdlvideo.red stub;
    List.iter ~f:(aux ~frame_color:Sdlvideo.black) stubs

let draw view
         ?draw_frame:(d_frame = false)
         ?draw_tiles:(d_tiles = false)
         ?draw_stubs:(d_stubs = false)
         ?draw_stub_frames:(d_stub_frames = false)
         room =
  List.iter ~f:(draw_layer view room.tileset) room.layers;
  if d_frame then draw_frame view room;
  if d_tiles then draw_tiles view room.tiles;
  if d_stubs then draw_stubs view room.stubs ~draw_frames:d_stub_frames