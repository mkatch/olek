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
    let (body, mindi, _) = Object.make (make_v x y) Sexp.unit mind in
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
  aux state []

let react env events state =
  let rec aux bodies minds = match bodies, minds with
    | [], [] -> ([], [], [])
    | body :: bodies, mind :: minds ->
      let bodies, minds, commandss = aux bodies minds in
      let body, mind, commands = Object.react env events body mind in
      (body :: bodies, mind :: minds, commands :: commandss)
    | _, _ -> failwith "Game.react: Uneven lists" in
  let bodies, minds, commandss = aux state.bodies state.minds in
  ({ state with bodies; minds }, commandss)

let think env commandss state =
  let rec aux bodies minds commandss = match bodies, minds, commandss with
    | [], [], [] -> ([], [], [])
    | body :: bodies, mind :: minds, commands :: commandss ->
      let bodies, minds, commandss = aux bodies minds commandss in
      let body, mind, commands = Object.think env body mind commands in
      (body :: bodies, mind :: minds, commands :: commandss)
    | _, _, _ -> failwith "Game.think: Uneven lists" in
  let bodies, minds, commandss = aux state.bodies state.minds commandss in
  ({ state with bodies; minds }, commandss)

let process_command body mind command state =
  let open Command in
  match command with
  | Print text -> print_endline text; state
  | Focus -> { state with view = View.focus (Body.pos body) state.view }

let process_commands commandss state =
  let rec aux bodies minds commandss state = match bodies, minds, commandss with
    | [], [], [] -> state
    | body :: bodies, mind :: minds, commands :: commandss ->
      let state = List.fold commands ~init:state
                            ~f:(fun s c -> process_command body mind c s) in
      aux bodies minds commandss state
    | _, _, _ -> failwith "Game.process_commands: Uneven lists" in
  aux state.bodies state.minds commandss state

let rec loop state =
  let env = env_of_state state in
  (* Advancing sprites here may seem out of place. But this is done in order
   * to prevent freshly set animations to be affected. *)
  let state = advance_sprites state in
  let state, obj_events = process_events state in
  let state, commandss = react env obj_events state in
  let state, commandss = think env commandss state in
  let state = process_commands commandss state in
  draw state;
  let state = update_time state in
  loop state

let main () =
  at_exit quit;
  loop (init ())

let () = main ()