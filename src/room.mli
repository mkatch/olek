open Utils

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
  layers : layer list;
}

val make_tileset : image_file:string -> tile_size:int -> tileset