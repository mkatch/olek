open Core.Std
open Utils

let name = "key"

type state = {
  olek : Env.handle;
}

type init = unit with sexp
type msg = unit with sexp

let sheet = Sprite.make_sheet ~image:"key" ~frames:1 ~dt:0 ~origin:(10, 6)

let default_body = Body.(make 0. 0. 20 12 |> set_sprite sheet)
let default_state = { olek = Env.Handle.nil; }
let default_init = ()

let init state body env init =
  let open Cmd in
  let open Context in
  let ctx = Env.context env in
  set_state { olek = Env.handle env "olek" } >>
  if ctx.has_key then remove_me else nop

let think state body env =
  let open Cmd in
  let open Context in
  let olek_body = Env.body env state.olek in
  if Body.intersect body olek_body then
    let aux ctx = { ctx with has_key = true } in
    alter_context aux >>
    remove_me
  else nop

let react state body env event = Cmd.nop
let receive state body env sender msg = Cmd.nop