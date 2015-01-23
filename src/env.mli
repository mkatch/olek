open Utils

type t

val t : t -> float

val dt : t -> float

val make : t:float -> dt:float -> tiles:Tile.t Grid.t -> t

val tile_at : vector -> t -> Tile.t