open Core.Std
open Utils

val init : w:int -> h:int -> unit

val dims : unit -> int * int

val flip : unit -> unit

val clear : Sdlvideo.color -> unit

val screenshot : unit -> Sdlvideo.surface

val blit : ?x:int -> ?y:int -> ?src_rect:Sdlvideo.rect -> Sdlvideo.surface
  -> unit

val draw_rect : Sdlvideo.rect -> Sdlvideo.color -> unit

val draw_filled_rect : Sdlvideo.rect -> Sdlvideo.color -> unit

val draw_text : int -> int -> ?fg:Sdlvideo.color -> ?bg:Sdlvideo.color -> string
  -> unit

val draw_body : View.t -> Body.t -> unit