type t with sexp

val row_cnt : int
val column_cnt : int
val width : int
val height : int

val tiles : t

val load : string -> t

val surface : t -> Sdlvideo.surface

val tile_rect : t -> int -> Sdlvideo.rect

val draw : t -> pos:int * int -> active:int -> unit