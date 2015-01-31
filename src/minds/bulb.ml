open Core.Std
open Utils

let name = "bulb"

type state = {
  mode : [`Idle | `Spit];
  t : float;
}

type init = unit with sexp
type msg = unit with sexp

let sheet = Sprite.make_sheet
  ~image:"bulb" ~frames:1 ~dt:0 ~origin:(21, 11)
let spit_sheet = Sprite.make_sheet
  ~image:"bulb_spit" ~frames:5 ~dt:80 ~origin:(21, 11)

let default_body = Body.(make 0. 0. 42 22 |> set_sprite sheet)
let default_state = {
  mode = `Idle;
  t = 0.;
}
let default_init = ()

let idle_dt = 5.0
let spit_dt = 0.4
let seed_vel = 300.0

let init state body env init =
  let open Cmd in
  let t = Env.t env in
  set_state { state with t }

let seed_init angle =
  let angle = Float.of_int angle /. 360.0 *. two_pi in
  let vel = seed_vel *.^ (cos angle, -.sin angle) in
  Seed.sexp_of_init vel

let think state body env =
  let open Cmd in
  let t = Env.t env in
  match state.mode with
  | `Idle when state.t +. idle_dt <= t ->
    set_state { mode = `Spit; t } >>
    set_body (Body.set_sprite spit_sheet body)
  | `Spit when state.t +. spit_dt <= t ->
    let seed_pos = (Body.pos body) -.^ (0., 11.) in
    spawn ~pos:seed_pos ~init:(seed_init  30) "seed" >>
    spawn ~pos:seed_pos ~init:(seed_init  60) "seed" >>
    spawn ~pos:seed_pos ~init:(seed_init  90) "seed" >>
    spawn ~pos:seed_pos ~init:(seed_init 120) "seed" >>
    spawn ~pos:seed_pos ~init:(seed_init 150) "seed" >>
    set_state { mode = `Idle; t } >>
    set_body (Body.set_sprite sheet body)
  | _ -> nop

let react state body env event = Cmd.nop
let receive state body env sender msg = Cmd.nop