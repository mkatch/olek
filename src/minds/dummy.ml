open Utils
open Mind
open Sdlkey

type state = unit
type msg = unit

let msg_from_int n = ()
let msg_to_int () = 0

let init body =
  Command.just_return Body.{ body with w = 50; h = 50 } ()

let think body state env =
  let incr_of_key_state k = if is_key_pressed k then 5. else 0. in
  let dl = incr_of_key_state KEY_LEFT in
  let du = incr_of_key_state KEY_UP in
  let dr = incr_of_key_state KEY_RIGHT in
  let dd = incr_of_key_state KEY_DOWN in
  let dp = make_v (dr -. dl) (dd -. du) in
  Command.just_return (Body.move_by body dp) state

let react body state event env =
  Command.just_return body state