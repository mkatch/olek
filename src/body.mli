open Core.Std
open Utils

type t

val make : vector -> int -> int -> t

val w : t -> int

val set_w : int -> t -> t

val h : t -> int

val set_h : int -> t -> t

val dims : t -> int * int

val set_dims : int -> int -> t -> t

val pos : t -> vector

val set_pos : vector -> t -> t

val move_by : vector -> t -> t

val sprite : t -> Sprite.instance

val set_sprite : Sprite.sheet -> ?offset:vector -> t -> t

val advance_sprite : int -> t -> t
(*
val rect : t -> Rect.t
*)
val sdl_rect : ?offset:vector -> t -> Sdlvideo.rect

val sprite_dst_sdl_rect : ?offset:vector -> t -> Sdlvideo.rect