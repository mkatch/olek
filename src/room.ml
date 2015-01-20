open Core.Std
open Utils

type tileset = {
  surface : Sdlvideo.surface;
  rows : int;
  cols : int;
  name : string;
}

let load_tileset name =
  let filename = filename_concat ["data"; "tilesets"; name ^ ".png"] in
  try
    let surface = Sdlloader.load_image filename in
    let w, h, _ = Sdlvideo.surface_dims surface in
    {
      surface = surface;
      rows = h / Tile.size;
      cols = w / Tile.size;
      name = name;
    }
  with Sdlloader.SDLloader_exception e -> failwith ("Room.load_tileset: " ^ e)

let sexp_of_tileset tileset = sexp_of_string tileset.name 

let tileset_of_sexp sexp = load_tileset (string_of_sexp sexp)

type layer =
  | Uniform of Sdlvideo.color
  | Tiled of int Grid.t
with sexp

type t = {
  tiles : Tile.t Grid.t;
  layers : layer list;
  tileset : tileset;
}
with sexp

let make rows cols =
  {
    tiles = Grid.make Tile.Void rows cols;
    layers = [];
    tileset = load_tileset "dummy";
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

let set_tileset name room = { room with tileset = load_tileset name }

let surface tileset = tileset.surface

let tileset_src_rect tileset k =
  let i = k / tileset.cols in
  let j = k mod tileset.cols in
  Sdlvideo.rect ~x:(j * Tile.size) ~y:(i * Tile.size) ~w:Tile.size ~h:Tile.size

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
(*
let input_uniform_layer inch =
  let color = In_channel.input_line inch
    |> option_value_exn
    |> String.split ~on:' '
    |> List.map ~f:Int.of_string
    |> triple_of_list_exn in
  ignore (In_channel.input_line inch);
  Uniform color

let input_layers inch =
  let rec aux rlayers =
    match In_channel.input_line inch with
    | None -> List.rev rlayers
    | Some layer_code ->
      match layer_code with
      | "u" -> aux (input_uniform_layer inch :: rlayers)
      | _ -> failwith ("Room.input_layers: Unknown layer code '" ^ layer_code ^
                       "'") in
  aux []
*)
let load name =
  let filename = filename_concat ["data"; "rooms"; name ^ ".orm"] in
  let file = In_channel.create filename in
  let tiles = input_tiles file in
  let layers = [] in
  { tiles; layers; tileset = (load_tileset "dummy") }

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