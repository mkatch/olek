open Core.Std
open Utils
open Mind

type t
type stub with sexp

val handle : t -> Env.handle
val name : t -> string option
val body : t -> Body.t
val set_body : Body.t -> t -> t

val make_stub : name:string option -> mind:Mind.mind -> pos:vector
  -> init:Sexp.t -> stub

val make : stub -> t * Cmd.t list

val think : Env.t -> t -> Cmd.t list -> t * Cmd.t list

val react : Env.t -> Objevent.t list -> t -> Cmd.t list -> t * Cmd.t list

val receive : Env.t -> Env.handle -> Sexp.t -> t -> t * Cmd.t list

val advance_sprite : int -> t -> t

val draw : View.t -> t -> unit

val for_env : t -> string option * Env.handle * Body.t