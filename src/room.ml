open Core.Std
open Utils

type tile =
  | Void
  | Solid
  | TopSolid
  | Sticky

type tileset = {
  surface : Sdlvideo.surface;
  rows : int;
  cols : int;
}

type layer =
  | Uniform of Sdlvideo.color
  | Tiled of tileset * int Grid.t

type t = {
  tiles : tile Grid.t;
  layers : layer list;
}

let tile_size = 16

let tiles room = room.tiles

let layers room = room.layers

let surface tileset = tileset.surface

let tileset_src_rect tileset k =
  let i = k / tileset.cols in
  let j = k mod tileset.cols in
  Sdlvideo.rect ~x:(j * tile_size) ~y:(i * tile_size) ~w:tile_size ~h:tile_size

let load_tileset name =
  let filename = filename_concat ["data"; "tilesets"; name ^ ".png"] in
  try
    let surface = Sdlloader.load_image filename in
    let w, h, _ = Sdlvideo.surface_dims surface in
    {
      surface = surface;
      rows = h / tile_size;
      cols = w / tile_size;
    }
  with Sdlloader.SDLloader_exception e -> failwith ("Room.make_tileset: " ^ e)

let tile_of_char = function
  | '#' -> Solid
  | '^' -> TopSolid
  | 'X' -> Sticky
  |  _  -> Void

let input_tiles inch =
  let rec aux rrows =
    let line = Option.value_exn (In_channel.input_line inch) in
    if String.is_empty line then List.rev rrows
    else
      let row = List.map ~f:tile_of_char (String.to_list line) in
      aux (row :: rrows) in
  let rows = aux [] in
  Printf.printf "rows: %d\n" (List.length rows);
  Grid.of_lists rows

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

let load name =
  let filename = filename_concat ["data"; "rooms"; name ^ ".orm"] in
  let file = In_channel.create filename in
  let tiles = input_tiles file in
  let layers = input_layers file in
  { tiles; layers }

let make_tiles_layer tiles =
  let aux = function
    | Void -> 0
    | Solid -> 2
    | TopSolid -> 4
    | Sticky -> 5 in
  let tileset = load_tileset "tiles" in
  let grid = Grid.map ~f:aux tiles in
  Tiled (tileset, grid)

let add_tiles_layer room =
  let tl = make_tiles_layer room.tiles in
  { room with layers = room.layers @ [tl] }