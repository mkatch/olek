open Utils

type t with sexp

type tileset

type layer =
  | Uniform of Sdlvideo.color
  | Tiled of int Grid.t

val make : int -> int -> t

val tiles : t -> Tile.t Grid.t

val layers : t -> layer list

val tileset : t -> tileset

val add_layer : layer -> ?i:int -> t -> t

val move_layer : src:int -> dst:int -> t -> t

val set_tileset : string -> t -> t

val surface : tileset -> Sdlvideo.surface

val tileset_src_rect : tileset -> int -> Sdlvideo.rect

val load : string -> t

val add_tiles_layer : t -> t