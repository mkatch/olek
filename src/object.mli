open Core.Std
open Utils
open Mind

type t

val body : t -> Body.t
val set_body : Body.t -> t -> t

val make : pos:vector -> init:Sexp.t -> (module MIND) -> t * Command.t list

val think : Env.t -> t -> Command.t list -> t * Command.t list

val react : Env.t -> Objevent.t list -> t -> t * Command.t list

val advance_sprite : int -> t -> t

val draw : View.t -> t -> unit