open Core.Std
open Utils

type t = {
  pos : vector;
  w : int;
  h : int;
}

val make : vector -> int -> int -> t

val move_by : t -> vector -> t

val move_to : t -> vector -> t

val to_rect : t -> Rect.t

val to_sdl_rect : ?offset:vector -> t -> Sdlvideo.rect