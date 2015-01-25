open Core.Std
open Utils
open Sdlkey

let name = "dummy"

type state = {
  dir : bool
}

type init = unit
with sexp

type msg = string with sexp

let run_right_sheet =
  Sprite.make_sheet ~image:"olek_run_right" ~frames:8 ~dt:60 ~origin:(0, 0)
let run_left_sheet =
  Sprite.make_sheet ~image:"olek_run_left"  ~frames:8 ~dt:60 ~origin:(0, 0)

let default_body = Body.make 0. 0. 20 20 |> Body.set_sprite run_right_sheet
let default_state = { dir = false }

let init state body init = Cmd.print "Init!"

let think state body env =
  let open Cmd in
  let incr_of_key_state k = if is_key_pressed k then 5. else 0. in
  let dl = incr_of_key_state KEY_LEFT in
  let du = incr_of_key_state KEY_UP in
  let dr = incr_of_key_state KEY_RIGHT in
  let dd = incr_of_key_state KEY_DOWN in
  let dp = (dr -. dl, dd -. du) in
  let body = Body.move_by dp body in
  let dir = dr > dl in
  let body =
    if dir = state.dir then body
    else
      let sheet = if dir then run_right_sheet else run_left_sheet in
      Body.set_sprite sheet body in 
  set { dir } body >> focus

let react state body env event =
  let me = Env.handle env "olek" in
  Cmd.send (sexp_of_msg "Heja banana!") me

let receive state body env sender msg =
  Cmd.print ("Message received: " ^ msg)