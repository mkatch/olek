open Core.Std
open Utils
open Sdlevent
open Sdlkey
open Printf
open Mind

type time = {
  frame : int;
  t_ms : int;
  dt_ms : int;
}

type state = {
  canvas : Canvas.t;
  room : Room.t;
  bodies : Body.t list;
  minds : mind list;
  time : time;
}

let rec make_objs n mind =
  if n <= 0 then []
  else
    let x = Random.float 800. and y = Random.float 600. in
    let (body, mindi, cmds) = Object.create (make_v x y) mind in
    (body, mindi) :: make_objs (n - 1) mind

let init ~w ~h ~fps =
  Sdl.init [`VIDEO];
  let canvas = Canvas.init ~w:w ~h:h in
  let objs = make_objs 1 Minds.dummy in
  let bodies, minds = List.unzip objs in
  let room = Room.of_file "data/room.orm" in
  let room = Room.add_tiles_layer room in
  let time = { 
    frame = 0;
    t_ms = Sdltimer.get_ticks ();
    dt_ms = 1000 / fps;
  } in {
    canvas = canvas;
    bodies = bodies;
    minds = minds;
    room = room;
    time = time;
  }

let quit () = Sdl.quit ()

let env_of_state state =
  let time = state.time in
  Env.{
    t = Float.of_int (time.frame * time.dt_ms) /. 1000.;
    dt = Float.of_int time.dt_ms /. 1000.;
    tiles = state.room.Room.tiles;
  }

let draw state =
  let c = state.canvas in
  Canvas.draw_room c state.room;
  List.iter ~f:(Canvas.draw_body c) state.bodies;
  Canvas.flip c

let update_time state =
  let time = state.time in
  let next_t_ms = time.t_ms + time.dt_ms in
  let current_t_ms = Sdltimer.get_ticks () in
  let delay_ms = max (next_t_ms - current_t_ms) 0 in
  Sdltimer.delay delay_ms;
  let new_t_ms = Sdltimer.get_ticks () in
  let new_time = { time with frame = time.frame + 1; t_ms = new_t_ms } in
  { state with time = new_time }

let advance_sprites state =
  let bodies = List.map ~f:(Body.advance_sprite state.time.t_ms) state.bodies in
  { state with bodies }

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
    let env = env_of_state state in
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
    Some { state with canvas = Canvas.focus (Body.pos b) c }

let iter state =
  draw state;
  let state = update_time state in
  let state = advance_sprites state in
  let state, obj_events = process_events state in
  let state = process_obj_events obj_events state in
  let state = think state in
  let state = focus state in
  state