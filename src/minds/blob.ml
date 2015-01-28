open Core.Std
open Utils

let name = "blob"

type state = {
  dir : [`Left | `Right];
  die_t : float;
  olek : Env.handle;
}

type init = unit with sexp
type msg = unit with sexp

let sheet =
  Sprite.make_sheet ~image:"blob" ~frames:4 ~dt:150 ~origin:(10, 7)
let die_sheet =
  Sprite.make_sheet ~image:"blob_die" ~frames:1 ~dt:0 ~origin:(11, -1)

let default_body = Body.(make 0. 0. 20 14 |> set_sprite sheet)
let default_state = {
  dir = `Left;
  die_t = -1.0;
  olek = Env.Handle.nil;
}
let default_init = ()

let vel_x = 40.0

let init state body env init =
  Cmd.set_state { state with olek = Env.handle env "olek" }

let handle_encounter state body env =
  let open Cmd in
  let olek_body = Env.body env state.olek in
  if Body.intersect body olek_body then
    let (x, y) = span (Body.pos olek_body) (Body.pos body) in
    if y > 2.0 *. Float.abs x then
      let dy = Body.t body -. Body.b olek_body in 
      let msg = Olek.sexp_of_msg (Olek.Bounce dy) in
      send msg state.olek >>
      set_state { state with die_t = Env.t env +. 1.0 } >>
      set_body (Body.set_sprite die_sheet body)
    else send (Olek.sexp_of_msg Olek.Die) state.olek
  else nop

let think state body env =
  let open Cmd in
  let dt = Env.dt env in
  let t = Env.t env in
  if state.die_t > 0. then
    if state.die_t <= t then remove_me else nop
  else (
  match state.dir with
  | `Left ->
    let body = Body.move_by (dt *. -.vel_x, 0.) body in
    set_body body >>
    if Env.leaning_left env body
    then set_state { state with dir = `Right }
    else nop
  | `Right ->
    let body = Body.move_by (dt *. vel_x, 0.) body in
    set_body body >>
    if Env.leaning_right env body
    then set_state { state with dir = `Left }
    else nop ) >>
  handle_encounter state body env

let react state body env event = Cmd.nop
let receive state body env sender msg = Cmd.nop