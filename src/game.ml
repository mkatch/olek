open Core.Std
open Utils
open Mind

type time = {
  frame : int;
  t_ms : int;
  dt_ms : int;
  last_fps_update_ms : int;
  frame_at_last_fps_update : int;
  fps : int;
}

type state = {
  room : Room.t;
  view : View.t;
  bodies : Body.t list;
  minds : mind list;
  time : time;
}

let window_width = 800
let window_height = 600
let fps = 30

let rec make_objs n mind =
  if n <= 0 then []
  else
    let x = Random.float 10. and y = Random.float 10. in
    let (body, mindi, cmds) = Object.create (make_v x y) mind in
    (body, mindi) :: make_objs (n - 1) mind

let init () =
  Sdl.init [`VIDEO];
  Sdlttf.init ();
  Canvas.init ~w:window_width ~h:window_height;
  let room_filename = filename_concat ["data"; "rooms"; "test.room"] in
  let room = Room.t_of_sexp (Sexp.load_sexp room_filename) in
  let objs = make_objs 1 Minds.dummy in
  let bodies, minds = List.unzip objs in
  let ticks = Sdltimer.get_ticks () in
  let time = { 
    frame = 0;
    t_ms = ticks;
    dt_ms = 1000 / fps;
    last_fps_update_ms = ticks;
    frame_at_last_fps_update = 0;
    fps = 0;
  } in {
    room = room;
    view = View.make Vector.nil;
    bodies = bodies;
    minds = minds;
    time = time;
  }

let quit () =
  Sdl.quit ();
  Sdlttf.quit ()

let env_of_state state =
  let time = state.time in
  Env.make
    ~t:(Float.of_int (time.frame * time.dt_ms) /. 1000.)
    ~dt:(Float.of_int time.dt_ms /. 1000.)
    ~tiles:(Room.tiles state.room)

let draw state =
  Canvas.clear Sdlvideo.gray;
  Room.draw state.room state.view ~draw_invisible:true;
  List.iter ~f:(fun b -> Body.draw b state.view ~draw_bbox:true) state.bodies;
  Canvas.flip ()

let update_time state =
  let time = state.time in
  let next_t_ms = time.t_ms + time.dt_ms in
  let current_t_ms = Sdltimer.get_ticks () in
  let delay_ms = max (next_t_ms - current_t_ms) 0 in
  Sdltimer.delay delay_ms;
  let t_ms = Sdltimer.get_ticks () in
  let frame = time.frame + 1 in
  let time = { time with frame; t_ms } in
  let time =
    if t_ms >= time.last_fps_update_ms + 1000 then
      let fps = frame - time.frame_at_last_fps_update in
      Printf.printf "fps: %d\n" fps; flush stdout;
      { time with
        fps = fps;
        frame_at_last_fps_update = frame;
        last_fps_update_ms = t_ms;
      }
    else time in
  { state with time }

let advance_sprites state =
  let bodies = List.map ~f:(Body.advance_sprite state.time.t_ms) state.bodies in
  { state with bodies }

let process_obj_events (state, events) =
  let env = env_of_state state in
  let objs_cmdss = List.map2_exn ~f:(Object.dispatch_events events env)
    state.bodies state.minds in
  let bodies, minds, cmdss = unzip3 objs_cmdss in
  { state with bodies; minds }

let process_event event state =
  let open Sdlevent in
  let open Sdlkey in
  match event with
  | QUIT
  | KEYDOWN {keysym = KEY_ESCAPE} -> exit 0
  | _ -> state

let process_events state =
  let rec aux state events =
    match Sdlevent.poll () with
    | None -> (state, List.(concat_map ~f:Objevent.of_sdl_event (rev events)))
    | Some event -> aux (process_event event state) (event :: events) in
  process_obj_events (aux state [])

let think state = process_obj_events (state, [Objevent.NextFrame])

let focus state =
  let center = Body.pos (List.hd_exn state.bodies) in
  { state with view = View.focus center state.view }

let rec loop state =
  draw state;
  state
  |> update_time
  |> advance_sprites
  |> process_events
  |> think
  |> focus
  |> loop

let main () =
  at_exit quit;
  loop (init ())

let () = main ()