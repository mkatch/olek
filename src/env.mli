open Utils

type t = {
  t : float;
  dt : float;
  tiles : Room.tile Grid.t;
}

val tile_at : vector -> t -> Room.tile