open Core.Std
open Utils
open Mind
open Sdlkey
open Env

type state = {
  dir : bool
}


type msg = unit

let msg_from_int n = ()
let msg_to_int () = 0

let run_right_sheet = Sprite.make_sheet ~image:"olek_run_right"
                                        ~frames:8 ~dt:60
let run_left_sheet  = Sprite.make_sheet ~image:"olek_run_left"
                                        ~frames:8 ~dt:60

let init body =
  let body = body |> Body.set_dims 50 50 |> Body.set_sprite run_right_sheet in
  let state = { dir = false } in
  Command.just_return body state

let think body state env =
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
  Command.just_return body { state with dir }

let react body state event env =
  Command.just_return body state