open Utils

module Handle : sig
  type t
  val compare : t -> t -> int
end

type t
type handle = Handle.t

val make : t_ms:int
        -> dt_ms:int
        -> context: Context.t
        -> tiles:Tile.t Grid.t
        -> objs:(string option * handle * Body.t) list
        -> t

val t : t -> float
val dt : t -> float
val context : t -> Context.t

val new_handle : unit -> handle
val handle : t -> string -> handle
val body : t -> handle -> Body.t
val named_body : t -> string -> Body.t

val tile : int * int -> t -> Tile.t
val tile_at : float * float -> t -> Tile.t
val tile_at_inclusive : float * float -> t -> Tile.t

val has_foundation : t -> Body.t -> bool
val leaning_left : t -> Body.t -> bool
val leaning_right : t -> Body.t -> bool
val penetr_left : float -> float
val penetr_right : float -> float

val collide : t -> Body.t -> float * float -> float * float * float * float