open Core.Std
open Utils

type t

val init : w:int -> h:int -> t

val dims : t -> int * int

val to_world : t -> vector -> vector

val to_canvas : t -> vector -> vector

val center : t -> vector

val focus : vector -> t -> t

val flip : t -> unit

val clear : t -> Sdlvideo.color -> unit

val draw_room : t -> Room.t -> unit

val draw_body : t -> Body.t -> unit