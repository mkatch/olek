open Core.Std
open Utils

let name = "checkpoint"

type state = {
  active : bool;
  olek : Env.handle;
}

type init = unit with sexp
type msg =
  | Deactivate
with sexp

let active_sheet = Sprite.make_sheet
  ~image:"checkpoint_active" ~frames:8 ~dt:60 ~origin:(10, 15)
let inactive_sheet = Sprite.make_sheet
  ~image:"checkpoint_inactive" ~frames:1 ~dt:0 ~origin:(10, 15)

let default_body = Body.(make 0. 0. 20 30 |> set_sprite inactive_sheet)
let default_state = {
  active = false;
  olek = Env.Handle.nil;
}
let default_init = ()

let init state body env init =
  Cmd.set_state { state with olek = Env.handle env "olek" }

let is_cp_name name = String.length name >= 2 && String.slice name 0 2 = "cp"

let think state body env =
  let open Cmd in
  let open Context in
  let olek_body = Env.body env state.olek in
  if (not state.active) && Body.intersect body olek_body then
    let name = Env.my_name_exn env in
    let ctx = Env.context env in
    set_state { state with active = true } >>
    set_body (Body.set_sprite active_sheet body) >>
    alter_context (fun ctx -> { ctx with spawn_point = name }) >>
    save >>
    if is_cp_name ctx.spawn_point then
      send (sexp_of_msg Deactivate) (Env.handle env ctx.spawn_point)
    else nop
  else nop

let react state body env event = Cmd.nop

let receive state body env sender msg =
  let open Cmd in
  match msg with
  | Deactivate ->
    set_state { state with active = false } >>
    set_body (Body.set_sprite inactive_sheet body)