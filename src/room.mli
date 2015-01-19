open Utils

type t

type tile =
  | Void
  | Solid
  | TopSolid
  | Sticky

type tileset

type layer =
  | Uniform of Sdlvideo.color
  | Tiled of tileset * int Grid.t

val tile_size : int

val tiles : t -> tile Grid.t

val layers : t -> layer list

val surface : tileset -> Sdlvideo.surface

val tileset_src_rect : tileset -> int -> Sdlvideo.rect

val load : string -> t

val add_tiles_layer : t -> t