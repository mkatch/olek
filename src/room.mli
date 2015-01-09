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

val of_file : string -> t

val make_tileset : image_file:string -> tile_size:int -> tileset