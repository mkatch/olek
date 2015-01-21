type t

val make : float -> float -> float -> float -> t

val l : t -> float

val t : t -> float

val r : t -> float

val b : t -> float

val w : t -> float

val h : t -> float

val coords : t -> float * float * float * float

val int_coords : t -> int * int * int * int

val to_sdl_rect : t -> Sdlvideo.rect