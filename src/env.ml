open Core.Std
open Utils

type t = {
  t : float;
  dt : float;
  tiles : Room.tile Grid.t;
}

let tile_at pos env =
  let i = (Float.to_int pos.Vector.y) / 16 in
  let j = (Float.to_int pos.Vector.x) / 16 in
  Grid.get i j env.tiles 