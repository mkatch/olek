open Utils

module Handle : sig
  type t
  val compare : t -> t -> int
end

type t
type handle = Handle.t

val t : t -> float

val dt : t -> float

val make : t_ms:int -> dt_ms:int -> room:Room.t ->
  objs:(string option * handle * Body.t) list -> t

val new_handle : unit -> handle

val tile_at : vector -> t -> Tile.t

val handle : t -> string -> handle

val body : t -> handle -> Body.t

val named_body : t -> string -> Body.t