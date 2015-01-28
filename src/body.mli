open Core.Std
open Utils

type t

val make : float -> float -> int -> int -> t

val x : t -> float
val y : t -> float
val pos : t -> float * float
val w : t -> int
val h : t -> int
val dims : t -> int * int
val l : t -> float
val t : t -> float
val r : t -> float
val b : t -> float
val lt : t -> float * float
val rt : t -> float * float
val rb : t -> float * float
val lb : t -> float * float
val sprite : t -> Sprite.t
val rect : t -> Sdlvideo.rect

val set_x : float -> t -> t
val set_y : float -> t -> t
val set_pos : float * float -> t -> t
val move_by : float * float -> t -> t
val set_w : int -> t -> t
val set_h : int -> t -> t
val set_dims : int * int -> t -> t
val set_sprite : Sprite.sheet -> ?force:bool -> t -> t

val advance_sprite : int -> t -> t

val intersect : t -> t -> bool

val draw : View.t -> t -> unit