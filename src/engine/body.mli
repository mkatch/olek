open Core.Std
open Utils

type t = {
  pos : Vector.t;
  w : int;
  h : int;
}

val make : Vector.t -> int -> int -> t

val move_by : t -> Vector.t -> t

val move_to : t -> Vector.t -> t

val to_rect : t -> Rect.t

val to_sdl_rect : t -> Sdlvideo.rect