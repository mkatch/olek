open Core.Std
open Utils
open Sdlevent
open Sdlkey
open Printf
open Mind

type state = {
  canvas : Canvas.t;
  room : Room.t;
  bodies : Body.t list;
  minds : mind list;
}

let draw state =
  let c = state.canvas in
  Canvas.draw_room c state.room;
  List.iter ~f:(Canvas.draw_body c) state.bodies;
  Canvas.flip c

let rec make_objs n mind =
  if n <= 0 then []
  else
    let x = Random.float 800. and y = Random.float 600. in
    let (body, mindi, cmds) = Object.create (make_v x y) mind in
    (body, mindi) :: make_objs (n - 1) mind

let init () =
  Sdl.init [`VIDEO];
  let canvas = Canvas.init ~w:800 ~h:600 in
  let objs = make_objs 1 Minds.dummy in
  let bodies, minds = List.unzip objs in
  let room = Room.of_file "data/room.orm" in
  let room = Room.add_tiles_layer room in {
    canvas = canvas;
    bodies = bodies;
    minds = minds;
    room = room;
  }

let quit () = Sdl.quit ()

let process_event state event =
  match event with
  | QUIT
  | KEYDOWN {keysym = KEY_ESCAPE} -> None
  | _ -> Some state

let process_events state =
  let rec aux state events =
    match Sdlevent.poll () with
    | None -> (Some state, List.(concat_map ~f:Objevent.of_sdl_event (rev events)))
    | Some event -> match process_event state event with
      | None -> (None, [])
      | Some state' -> aux state' (event :: events) in
  aux state []

let process_obj_events events = function
  | None -> None
  | Some state ->
    let env = Env.dummy in
    let objs_cmdss = List.map2_exn ~f:(Object.dispatch_events events env)
      state.bodies state.minds in
    let bodies, minds, cmdss = unzip3 objs_cmdss in
    Some { state with bodies = bodies; minds = minds }

let think = process_obj_events [Objevent.NextFrame]

let focus = function
  | None -> None
  | Some state ->
    let c = state.canvas in
    let b = List.hd_exn state.bodies in
    Some { state with canvas = Canvas.focus b.Body.pos c }

let iter state =
  draw state;
  let state, obj_events = process_events state in
  let state = process_obj_events obj_events state in
  let state = think state in
  let state = focus state in
  state