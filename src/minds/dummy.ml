open Core.Std
open Utils
open Sdlkey

let name = "dummy"

type state = {
  dir : bool
}

module Msg = struct type t = unit with sexp end

type init = unit
with sexp

let run_right_sheet =
  Sprite.make_sheet ~image:"olek_run_right" ~frames:8 ~dt:60 ~origin:(0, 0)
let run_left_sheet =
  Sprite.make_sheet ~image:"olek_run_left"  ~frames:8 ~dt:60 ~origin:(0, 0)

let default_body = Body.make Vector.nil 20 20 |> Body.set_sprite run_right_sheet
let default_state = { dir = false }

let init state body init = Cmd.print "Init!"

let think state body env =
  let open Cmd in
  let incr_of_key_state k = if is_key_pressed k then 5. else 0. in
  let dl = incr_of_key_state KEY_LEFT in
  let du = incr_of_key_state KEY_UP in
  let dr = incr_of_key_state KEY_RIGHT in
  let dd = incr_of_key_state KEY_DOWN in
  let dp = make_v (dr -. dl) (dd -. du) in
  let body = Body.move_by dp body in
  let dir = dp.x > 0. in
  let body =
    if dir = state.dir then body
    else
      let sheet = if dir then run_right_sheet else run_left_sheet in
      Body.set_sprite sheet body in 
  set { state with dir } body >> focus

let react state body env event =
  let b = Env.named_body env "olek" in
  let (x, y) = Vector.to_ints (Body.pos b) in
  Cmd.print (Int.to_string x ^ " " ^ Int.to_string y)

let receive state body env sender msg = Cmd.print "Message received!"