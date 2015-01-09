open Core.Std
open Utils

type tile =
  | Void
  | Solid
  | TopSolid
  | SidesSticky
  | LeftSticky
  | RightSticky
  | FrontSticky

type tileset = {
  image : Sdlvideo.surface;
  tile_size : int;
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

let tile_of_char = function
  | '#' -> Solid
  | '^' -> TopSolid
  | 'X' -> SidesSticky
  | '>' -> LeftSticky
  | '<' -> RightSticky
  | '-' -> FrontSticky
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

let make_tileset ~image_file ~tile_size =
  try
    let image = Sdlloader.load_image image_file in
    let w, h, _ = Sdlvideo.surface_dims image
    in {
      image = image;
      tile_size = tile_size;
      rows = h / tile_size;
      cols = w / tile_size;
    }
  with Sdlloader.SDLloader_exception e -> failwith ("make_tileset: " ^ e) 

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

let of_file filename =
  let file = In_channel.create filename in
  let tiles = input_tiles file in
  let layers = input_layers file in
  { tiles; layers }