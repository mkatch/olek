open Utils

type t with sexp

type layer =
  | Uniform of Sdlvideo.color
  | Tiled of int Grid.t

val make : int -> int -> t

val tiles : t -> Tile.t Grid.t

val layers : t -> layer list

val tileset : t -> Tileset.t

val add_layer : layer -> ?i:int -> t -> t

val move_layer : src:int -> dst:int -> t -> t

val set_tileset : string -> t -> t

val load : string -> t

val add_tiles_layer : t -> t

val draw : t -> View.t -> unit