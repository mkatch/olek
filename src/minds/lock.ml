open Core.Std
open Utils

let name = "lock"

type state = { olek : Env.handle }
type init = unit with sexp
type msg = unit with sexp

let sheet = Sprite.make_sheet ~image:"lock" ~frames:1 ~dt:0 ~origin:(16, 24)

let default_body = Body.(make 0. 0. 32 48 |> set_sprite sheet)
let default_state = { olek = Env.Handle.nil }
let default_init = ()

let init state body env init =
  let open Cmd in
  let open Context in
  let ctx = Env.context env in
  set_state { olek = Env.handle env "olek" } >>
  if ctx.has_key then remove_me else nop

let think state body env =
  let open Cmd in
  let olek_body = Env.body env state.olek in
  if Body.intersect olek_body body then
    let dir = sign (Body.x olek_body -. Body.x body) in
    let dx = 0.5 *. Float.of_int (Body.w body + Body.w olek_body) in
    let x = Body.x body +. dir *. dx in
    let msg = Olek.sexp_of_msg (Olek.MoveTo (x, Body.y olek_body)) in
    send msg state.olek
  else nop

let react state body env event = Cmd.nop
let receive state body env sender msg = Cmd.nop