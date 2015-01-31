open Utils

type t

val make : int * int -> Sdlvideo.rect -> t

val offset : t -> int * int

val center : t -> int * int

val to_world : t -> int * int -> int * int
val to_view : t -> int * int -> int * int

val focus : int * int -> ?dt:float -> t -> t

val move_by : int * int -> t -> t

val blit : t -> ?pos:int * int -> ?src_rect:Sdlvideo.rect -> Sdlvideo.surface
  -> unit

val draw_rect : t -> Sdlvideo.color -> Sdlvideo.rect -> unit

val draw_text : t -> int * int -> Sdlvideo.color -> string -> unit