open Core.Std
open Utils

type image_layer = {
  surface: Sdlvideo.surface;
  offset: int * int;
  repeat: bool * bool;
  parallax: float;
  image_name: string;
}

let make_image_layer ~name ~offset ~repeat ~parallax =
  let filename = filename_concat ["data"; "backgrounds"; name ^ ".png"] in
  let surface = Sdlloader.load_image filename in
  { surface; offset; repeat; parallax; image_name = name }

let sexp_of_image_layer il =
  let open Sexp in
  let (ox, oy) = il.offset in
  let (rx, ry) = il.repeat in
  List [
    List [Atom "name"; Atom il.image_name];
    List [Atom "offset"; Atom (Int.to_string ox); Atom (Int.to_string ox)];
    List [Atom "repeat"; Atom (Bool.to_string rx); Atom (Bool.to_string ry)];
    List [Atom "parallax"; Atom (Float.to_string il.parallax)]
  ]

let image_layer_of_sexp sexp =
  let open Sexp in
  match sexp with
  | List [
      List [Atom "name"; Atom name];
      List [Atom "offset"; Atom ox; Atom oy];
      List [Atom "repeat"; Atom rx; Atom ry];
      List [Atom "parallax"; Atom parallax];
    ] -> make_image_layer ~name:name
                          ~offset:(Int.of_string ox, Int.of_string oy)
                          ~repeat:(Bool.of_string rx, Bool.of_string ry)
                          ~parallax:(Float.of_string parallax)
  | _ -> failwith "Room.image_layer_of_sexp: Malformed s-expression"

type layer =
  | Uniform of Sdlvideo.color
  | Image of image_layer
  | Tiled of int Grid.t
with sexp

type t = {
  name : string;
  tiles : Tile.t Grid.t;
  layers : layer list;
  tileset : Tileset.t;
  stubs : Object.stub list;
}
with sexp

let make name row_cnt column_cnt = {
  name = name;
  tiles = Grid.make Tile.Void row_cnt column_cnt;
  layers = [];
  tileset = Tileset.load "dummy";
  stubs = [];
}

let name room = room.name
let tiles room = room.tiles
let layers room = room.layers
let tileset room = room.tileset
let stubs room = room.stubs
let layer_cnt room = List.length room.layers
let row_cnt room = Grid.row_cnt room.tiles
let column_cnt room = Grid.column_cnt room.tiles
let dims room = Grid.dims room.tiles
let dims_px room = Tile.size *^ dims room

let layer_is_image room i =
  match List.nth room.layers i with
  | Some (Image _) -> true
  | _ -> false

let layer_is_tiled room i =
  if i = (-1) then true
  else match List.nth room.layers i with
    | Some (Tiled _) -> true
    | _ -> false

let add_uniform_layer color room =
  { room with layers = Uniform color :: room.layers }

let add_image_layer name room =
  let il = make_image_layer ~name:name
                            ~offset:(0, 0)
                            ~repeat:(true, true)
                            ~parallax:1.0 in
  { room with layers = Image il :: room.layers }

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

let rem_layer i room = { room with layers = list_rem i room.layers }

let set_name name room = { room with name }
let set_tileset name room = { room with tileset = Tileset.load name }

let map_image_layer f layer room =
  let rec aux n layers = match n, layers with
    | _, [] -> failwith "Room.map_image_layer: No shuch layer"
    | 0, Image il :: layers -> Image (f il) :: layers
    | 0, _ -> failwith "Room.map_image_layer: Cannot act upon non image layer"
    | n, layer :: layers -> layer :: aux (n - 1) layers in
  { room with layers = aux layer room.layers }

let move_image_layer_by doffset layer room =
  let aux il = { il with offset = il.offset +^ doffset } in
  map_image_layer aux layer room

let set_image_layer_repeat repeat layer room =
  let aux il = { il with repeat } in
  map_image_layer aux layer room

let set_image_layer_parallax parallax layer room =
  let aux il = { il with parallax } in
  map_image_layer aux layer room

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

let rem_selected_stub room =
  match room.stubs with
  | [] -> room
  | _ :: stubs -> { room with stubs }

let draw_uniform_layer color = Canvas.clear color

let draw_image_layer view layer =
  let (x0, y0) = v_to_ints (layer.parallax *.^ v_to_floats (View.offset view))
              +^ layer.offset in
  let (repeat_x, repeat_y) = layer.repeat in
  if repeat_x = false && repeat_y = false then
    Canvas.blit ~pos:(x0, y0) layer.surface
  else
    let w, h = Canvas.dims () in
    let dx, dy, _ = Sdlvideo.surface_dims layer.surface in
    let x0 =
      if repeat_x then (let x = actual_mod x0 dx in if x > 0 then x - dx else x)
      else x0 in
    let y0 =
      if repeat_y then (let y = actual_mod y0 dy in if y > 0 then y - dy else y)
      else y0 in
    let make_x y =
      if repeat_x then
        let x = ref x0 in
        while !x < w do
          Canvas.blit ~pos:(!x, y) layer.surface;
          x := !x + dx
        done else
      if x0 < w && x0 + dx > 0 then
        Canvas.blit ~pos:(x0, y) layer.surface in
    if repeat_y then
      let y = ref y0 in
      while !y < h do
        make_x !y;
        y := !y + dy
      done else
    make_x y0

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
  | Image il -> draw_image_layer view il
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
         ?only:(only = -2)
         room =
  if only >= 0 then
    let layer = List.nth_exn room.layers only in
    draw_layer view room.tileset layer else
  if only <> -1 then
    List.iter ~f:(draw_layer view room.tileset) room.layers;
  if d_frame then draw_frame view room;
  if d_tiles && only < 0 then draw_tiles view room.tiles;
  if d_stubs then draw_stubs view room.stubs ~draw_frames:d_stub_frames