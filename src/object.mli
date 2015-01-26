open Core.Std
open Utils
open Mind

type t
type stub with sexp

val handle : t -> Env.handle
val name : t -> string option
val body : t -> Body.t
val set_body : Body.t -> t -> t

val make_stub : name:string option -> mind:Mind.mind -> pos:int * int
  -> init:Sexp.t -> stub

val make : stub -> t

val init : Env.t -> stub -> t -> t * Cmd.t list
val think : Env.t -> t -> Cmd.t list -> t * Cmd.t list
val react : Env.t -> Objevent.t list -> t -> Cmd.t list -> t * Cmd.t list
val receive : Env.t -> Env.handle -> Sexp.t -> t -> Cmd.t list -> t * Cmd.t list

val advance_sprite : int -> t -> t

val draw : View.t -> t -> unit

val stub_init : stub -> Sexp.t
val stub_mind : stub -> Mind.mind
val stub_contains : int * int -> stub -> bool

val set_stub_name : string option -> stub -> stub
val set_stub_init : Sexp.t -> stub -> stub
val move_stub_by : int * int -> stub -> stub

val draw_stub : View.t -> ?draw_frame:bool -> ?frame_color:Sdlvideo.color
  -> stub -> unit

val for_env : t -> string option * Env.handle * Body.t