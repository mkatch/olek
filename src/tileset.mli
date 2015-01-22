type t with sexp

val column_cnt : int
val row_cnt : int

val tiles : t

val load : string -> t

val surface : t -> Sdlvideo.surface

val tile_rect : t -> int -> Sdlvideo.rect

val draw : t -> x:int -> y:int -> active:int -> unit