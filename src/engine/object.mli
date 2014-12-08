open Core.Std
open Utils

module type Mind = sig
  type t
  type obj = { body : Body.t; mind : t }
  val init : Body.t -> obj
  val think : obj -> obj
end

type t

val create : Vector.t -> (module Mind) -> t

val body : t -> Body.t

val think : t -> t