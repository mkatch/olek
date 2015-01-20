open Utils

type t

val t : t -> float

val dt : t -> float

val make : t:float -> dt:float -> tiles:Room.tile Grid.t -> t

val tile_at : vector -> t -> Room.tile