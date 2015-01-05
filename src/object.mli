open Utils
open Mind

val create : vector -> (module MIND) -> (Body.t * mind * Command.t list)

val dispatch_events : Objevent.t list -> Env.t -> Body.t -> mind
  -> (Body.t * mind * Command.t list)