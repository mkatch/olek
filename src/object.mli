open Core.Std
open Utils
open Mind

type t
type stub with sexp

val name : t -> string option
val body : t -> Body.t
val set_body : Body.t -> t -> t

val make_stub : pos:vector -> mind:string -> init:Sexp.t -> stub

val make : ?name:string -> stub -> t * Command.t list

val think : Env.t -> t -> Command.t list -> t * Command.t list

val react : Env.t -> Objevent.t list -> t -> t * Command.t list

val advance_sprite : int -> t -> t

val draw : View.t -> t -> unit

val for_env : t -> string option * Env.handle * Body.t