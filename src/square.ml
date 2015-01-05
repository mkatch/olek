open Utils
open Mind

type state = {
  c: vector;
  r: float;
  a: float;
  da: float
}

type msg = unit

let msg_from_int n = ()

let msg_to_int () = 0

let init body =
  let body = Body.{ body with
    w = Random.int 3 + 1;
    h = Random.int 3 + 1;
  } in
  let state = {
    c = body.Body.pos;
    r = Random.float 20. +. 30.;
    a = 0.;
    da = Random.float 0.1;
  } in
  Command.just_return body state

let think body state env =
  let s = sin state.a in
  let c = cos state.a in
  let offset = state.r *^ make_v s c in
  let body = Body.move_to body (state.c +^ offset) in
  let state = { state with a = state.a +. state.da } in
  Command.just_return body state

let react body state event env =
  Command.just_return body { state with c = state.c +^ (make_v 1. 0.) }