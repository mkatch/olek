open Core.Std
open Utils
open Mind
open Sdlkey
open Env

type state = unit
type msg = unit

let msg_from_int n = ()
let msg_to_int () = 0

let init body =
  let body = Body.{ body with
      w = 50;
      h = 50;
      color = Sdlvideo.blue;
    } in
  let mind = () in
  Command.just_return body mind

let think body state env =
  let incr_of_key_state k = if is_key_pressed k then 5. else 0. in
  let dl = incr_of_key_state KEY_LEFT in
  let du = incr_of_key_state KEY_UP in
  let dr = incr_of_key_state KEY_RIGHT in
  let dd = incr_of_key_state KEY_DOWN in
  let dp = make_v (dr -. dl) (dd -. du) in
  let tile = Env.tile_at body.Body.pos env in
  let color = if tile = Room.Void then Sdlvideo.black else Sdlvideo.white in
  let body = Body.move_by body dp in
  let body = Body.{ body with color } in
  Command.just_return body state

let react body state event env =
  Command.just_return body state