open Core.Std
open Utils

let name = "orb"

type state = {
  olek : Env.handle;
}

type init = unit with sexp
type msg = unit with sexp

let sheet = Sprite.make_sheet ~image:"orb" ~frames:8 ~dt:300 ~origin:(11, 13)

let default_body = Body.(make 0. 0. 22 26 |> set_sprite sheet)
let default_state = {
  olek = Env.Handle.nil;
}
let default_init = ()

let init state body env init =
  Cmd.set_state { olek = Env.handle env "olek" }

let think state body env =
  let open Cmd in
  let olek_body = Env.body env state.olek in
  if Body.intersect body olek_body then quit else nop

let react state body env event = Cmd.nop
let receive state body env sender msg = Cmd.nop