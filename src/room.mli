open Utils

type tile =
  | Void
  | Solid
  | TopSolid
  | Sticky

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

val of_file : string -> t

val add_tiles_layer : t -> t