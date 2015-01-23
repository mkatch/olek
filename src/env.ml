open Core.Std
open Utils

type t = {
  t : float;
  dt : float;
  tiles : Tile.t Grid.t;
}

let t env = env.t

let dt env = env.dt

let make ~t ~dt ~tiles = { t; dt; tiles; }

let tile_at pos env =
  let i = (Float.to_int pos.Vector.y) / 16 in
  let j = (Float.to_int pos.Vector.x) / 16 in
  Grid.get i j env.tiles