open Utils

type t

val make : vector -> t

val offset : t -> vector

val int_offset : t -> int * int

val to_world : t -> int * int -> int * int

val to_view : t -> int * int -> int * int

val focus : vector -> ?dt:float -> t -> t

val move_by : int * int -> t -> t

val blit : t -> ?x:int -> ?y:int -> ?src_rect:Sdlvideo.rect -> Sdlvideo.surface
  -> unit

val draw_rect : t -> Sdlvideo.rect -> Sdlvideo.color -> unit