open Core.Std
open Utils

let name = "gate"


type init = {
  mode : [`Left | `Right];
  dst_room : string;
  dst_spawn_point : string;
}
with sexp

type state = {
  olek : Env.handle;
  init : init;
}
type msg = unit with sexp

let sheet = Sprite.make_sheet ~image:"gate" ~frames:1 ~dt:0 ~origin:(8, 10)

let default_body = Body.(make 0. 0. 16 20 |> set_sprite sheet)
let default_init = {
  mode = `Left;
  dst_room = "";
  dst_spawn_point = "";
}
let default_state = {
  olek = Env.Handle.nil;
  init = default_init;
}

let init state body env init =
  let open Cmd in
  set_state { olek = Env.handle env "olek"; init } >>
  set_body (Body.set_visible false body)

let think state body env =
  let open Cmd in
  let olek_body = Env.body env state.olek in
  let warp = match state.init.mode with
  | `Left -> (Body.r olek_body <= Body.r body)
  | `Right -> (Body.l olek_body >= Body.l body) in
  if warp then
    let open Context in
    let aux ctx = { ctx with spawn_point = state.init.dst_spawn_point} in
    alter_context aux >>
    set_room state.init.dst_room
  else nop

let react state body env event = Cmd.nop
let receive state body env sender msg = Cmd.nop 