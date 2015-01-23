open Core.Std
open Utils
open Mind

val make : pos:vector -> init:Sexp.t -> (module MIND)
  -> (Body.t * mind * Command.t list)

val think : Env.t -> Body.t -> mind -> Command.t list
  -> (Body.t * mind * Command.t list)

val react : Env.t -> Objevent.t list -> Body.t -> mind
  -> (Body.t * mind * Command.t list)