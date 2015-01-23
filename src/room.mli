open Utils

type t with sexp

type layer =
  | Uniform of Sdlvideo.color
  | Tiled of int Grid.t

val tiles : t -> Tile.t Grid.t

val layers : t -> layer list

val tileset : t -> Tileset.t

val layer_cnt : t -> int

val row_cnt : t -> int

val column_cnt : t -> int

val dims : t -> int * int

val dims_px : t -> int * int

val layer_is_tiled : t -> int -> bool

val make : int -> int -> t

val add_uniform_layer : Sdlvideo.color -> t -> t

val add_tiled_layer : t -> t

val move_layer : src:int -> dst:int -> t -> t

val set_tileset : string -> t -> t

val put_tile : int -> int -> layer:int -> tile:int -> t -> t

val draw : t -> ?draw_invisible:bool -> View.t -> unit