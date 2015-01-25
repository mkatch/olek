open Core.Std
open Utils

val init : w:int -> h:int -> unit

val dims : unit -> int * int

val flip : unit -> unit

val clear : Sdlvideo.color -> unit

val screenshot : unit -> Sdlvideo.surface

val blit : ?pos:int * int -> ?src_rect:Sdlvideo.rect -> Sdlvideo.surface -> unit

val draw_rect : Sdlvideo.color -> Sdlvideo.rect -> unit

val draw_filled_rect : Sdlvideo.color -> Sdlvideo.rect  -> unit

val draw_text : int * int -> Sdlvideo.color -> string -> unit

val size_text : string -> int * int