open Core.Std
open Utils
open Sdlkey

let name = "olek"

type state = {
  vel : float * float;
  dir : [`Left | `Right];
  mode : [`Ground | `Air];
}

type init = unit with sexp
type msg = unit with sexp

let still_left_sheet =
  Sprite.make_sheet ~image:"olek_idle_left" ~frames:1 ~dt:0 ~origin:(9, 9)
let still_right_sheet =
  Sprite.make_sheet ~image:"olek_idle_right" ~frames:1 ~dt:0 ~origin:(16, 9)
let run_left_sheet =
  Sprite.make_sheet ~image:"olek_run_left" ~frames:8 ~dt:60 ~origin:(9, 12)
let run_right_sheet =
  Sprite.make_sheet ~image:"olek_run_right" ~frames:8 ~dt:60 ~origin:(20, 12)

let default_body = Body.(make 0. 0. 12 16 |> set_sprite still_right_sheet) 
let default_state = {
  vel = (0., 0.);
  dir = `Right;
  mode = `Ground;
}
let default_init = ()

let acc_x = 1000.0
let jump_vel_y = -200.0
let drag_x = 1000.0
let grav_y = 600.0
let max_vel_x = 150.0
let max_vel_y = 200.0

let float_of_ks k = if is_key_pressed k then 1. else 0.

let determine_sprite (vel_x, vel_y) dir =
  if vel_x < -3.0 then run_left_sheet else
  if 3.0 < vel_x then run_right_sheet else
  if dir = `Left then still_left_sheet
  else still_right_sheet

let init state body env init = Cmd.nop

let determine_vel_x vel_x dt =
  let dir_x = float_of_ks KEY_RIGHT -. float_of_ks KEY_LEFT in
  if dir_x <> 0. then
    let acc_x = acc_x *. dir_x in
    clamp ~min:(-.max_vel_x) ~max:max_vel_x (vel_x +. dt *. acc_x) else
  if vel_x <= 0. then
    clamp ~min:(-.max_vel_x) ~max:0. (vel_x +. dt *. drag_x)
  else
    clamp ~min:0. ~max:max_vel_x (vel_x +. dt *. (-.drag_x))

let determine_vel_y vel_y dt mode =
  match mode with
  | `Ground -> 0.
  | `Air ->
    let grav_y = if is_key_pressed KEY_SPACE then 0.7 *. grav_y else grav_y in
    clamp ~min:(-.max_vel_y) ~max:max_vel_y (vel_y +. dt *. grav_y)

let handle_collisions state body env =
  let open Cmd in
  let (vel_x, vel_y) as vel = state.vel in
  match state.mode with
  | `Ground -> let state, body =
    if vel_x < 0. && Env.leaning_left env body then
      let dx = Env.penetr_left (Body.l body) in
      { state with vel = (0., vel_y) }, Body.move_by (dx, 0.) body else
    if vel_x > 0. && Env.leaning_right env body then
      let dx = -.Env.penetr_right (Body.r body) in
      { state with vel = (0., vel_y) }, Body.move_by (dx, 0.) body
    else state, body in
    let state =
      if not (Env.has_foundation env body) then { state with mode = `Air }
      else state in
    set_state state >> set_body body
  | `Air ->
    let (pl, pt, pr, pb) = Env.collide env body vel in
    set_body (Body.move_by (pl -. pr, pt -. pb) body) >>
    if pb > 0. then set_state { state with mode = `Ground; vel = (vel_x, 0.) }
    else nop

let think state body env =
  let dt = Env.dt env in
  let vel_x = determine_vel_x (vx state.vel) dt in
  let vel_y = determine_vel_y (vy state.vel) dt state.mode in
  let vel = (vel_x, vel_y) in
  let state = { state with vel } in
  let body = body
             |> Body.move_by (dt *.^ vel)
             |> Body.set_sprite (determine_sprite vel state.dir) in
  let open Cmd in
  set_state state >>
  set_body body >>
  handle_collisions state body env >>
  focus

let react state body env event =
  let open Cmd in
  let open Objevent in
  match event with
  | KeyDown KEY_LEFT -> set_state { state with dir = `Left }
  | KeyDown KEY_RIGHT -> set_state { state with dir = `Right }
  | KeyDown KEY_SPACE when state.mode = `Ground ->
    let vel_x = vx state.vel in
    set_state { state with vel = (vel_x, jump_vel_y); mode = `Air }
  | _ -> nop

let receive state body env sender msg = Cmd.nop