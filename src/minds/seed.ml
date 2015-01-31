open Core.Std
open Utils

let name = "seed"

type state = {
  vel : float * float;
  ttl : float;
  olek : Env.handle;
}

type init = float * float with sexp
type msg = unit with sexp

let sheet = Sprite.make_sheet ~image:"seed" ~frames:1 ~dt:0 ~origin:(4, 4)

let default_body = Body.(make 0. 0. 8 8 |> set_sprite sheet)
let default_state = {
  vel = (0., 0.);
  ttl = 3.0;
  olek = Env.Handle.nil;
}
let default_init = (0., 0.)

let init state body env init =
  Cmd.set_state { state with
    vel = init;
    olek = Env.handle env "olek";
  }

let grav = (0.0, 600.0)

let think state body env =
  let open Cmd in
  let olek_body = Env.body env state.olek in
  if Body.intersect olek_body body then
    let msg = Olek.sexp_of_msg Olek.Die in
    send msg state.olek >>
    remove_me else
  if state.ttl <= 0. || Tile.is_solid (Env.tile_at (Body.pos body) env) then
    remove_me
  else
    let dt = Env.dt env in
    let dpos = dt *.^ state.vel in
    let dvel = dt *.^ grav in
    set_state { state with vel = state.vel +.^ dvel; ttl = state.ttl -. dt } >>
    set_body (Body.move_by dpos body)

let react state body env event = Cmd.nop
let receive state body env sender msg = Cmd.nop