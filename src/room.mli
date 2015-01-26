open Utils

type t with sexp
type image_layer

type layer =
  | Uniform of Sdlvideo.color
  | Image of image_layer
  | Tiled of int Grid.t

val make : string -> int -> int -> t

val name : t -> string
val tiles : t -> Tile.t Grid.t
val layers : t -> layer list
val tileset : t -> Tileset.t
val stubs : t -> Object.stub list
val layer_cnt : t -> int
val row_cnt : t -> int
val column_cnt : t -> int
val dims : t -> int * int
val dims_px : t -> int * int

val layer_is_image : t -> int -> bool
val layer_is_tiled : t -> int -> bool

val add_uniform_layer : Sdlvideo.color -> t -> t
val add_image_layer : string -> t -> t
val add_tiled_layer : t -> t

val move_layer : src:int -> dst:int -> t -> t
val rem_layer : int -> t -> t

val set_name : string -> t -> t
val set_tileset : string -> t -> t

val move_image_layer_by : int * int -> int -> t -> t
val set_image_layer_repeat : bool * bool -> int -> t -> t
val set_image_layer_parallax : float -> int -> t -> t
val put_tile : int -> int -> layer:int -> tile:int -> t -> t

val add_stub : Object.stub -> t -> t
val select_stub : int * int -> t -> t * bool
val map_selected_stub : f:(Object.stub -> Object.stub) -> t -> t
val rem_selected_stub : t -> t

val draw : View.t -> ?draw_frame:bool -> ?draw_tiles:bool -> ?draw_stubs:bool
  -> ?draw_stub_frames:bool -> ?only:int -> t -> unit