open Core.Std
open Utils

let name = "blob"

type state = {
  dir : [`Left | `Right];
  olek : Env.handle;
}

type init = unit with sexp
type msg = unit with sexp

let sheet = Sprite.make_sheet ~image:"blob" ~frames:4 ~dt:150 ~origin:(10, 7)

let default_body = Body.(make 0. 0. 20 14 |> set_sprite sheet)
let default_state = {
  dir = `Left;
  olek = Env.Handle.nil;
}
let default_init = ()

let vel_x = 40.0

let init state body env init =
  Cmd.set_state { state with olek = Env.handle env "olek" }

let handle_encounter state body env =
  let olek_body = Env.body env state.olek in
  if dist (Body.pos olek_body) (Body.pos body) < 10.0 then
    print_endline "Whaa!";
  Cmd.nop

let think state body env =
  let open Cmd in
  let dt = Env.dt env in (
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