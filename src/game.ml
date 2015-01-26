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

type message = {
  sender: Env.handle;
  receiver: Env.handle;
  data: Sexp.t;
}

type state = {
  room : Room.t;
  view : View.t;
  objs : Object.t list;
  context : Context.t;
  time : time;
  messages : message list;
  pending_inits : Object.stub list;
}

type save = {
  room_name : string;
  context : Context.t;
}
with sexp

let window_width = 800
let window_height = 600
let fps_cap = 30

let init ~save:save =
  Sdl.init [`VIDEO];
  Sdlttf.init ();
  Canvas.init ~w:window_width ~h:window_height;
  let room_filename =
    filename_concat ["data"; "rooms"; save.room_name ^ ".room"] in
  let room = Room.t_of_sexp (Sexp.load_sexp room_filename) in
  let stubs = Room.stubs room in
  let ticks = Sdltimer.get_ticks () in
  let time = { 
    frame = 0;
    t_ms = ticks;
    dt_ms = 1000 / fps_cap;
    last_fps_update_ms = ticks;
    frame_at_last_fps_update = 0;
    fps = 0;
  } in {
    room = room;
    view = View.make (0, 0);
    objs = List.map ~f:Object.make stubs;
    context = save.context;
    time = time;
    messages = [];
    pending_inits = stubs;
  }

let quit () =
  Sdl.quit ();
  Sdlttf.quit ()

let env_of_state state =
  let time = state.time in
  Env.make
    ~t_ms:(time.frame * time.dt_ms)
    ~dt_ms:time.dt_ms
    ~context:(state.context)
    ~tiles:(Room.tiles state.room)
    ~objs:(List.map ~f:Object.for_env state.objs)

let resolve_pending_inits env state =
  let rec aux objs stubs = match objs, stubs with
    | objs, [] -> List.map ~f:(fun obj -> (obj, [])) objs
    | obj :: objs, stub :: stubs -> Object.init env stub obj :: aux objs stubs
    | _, _ -> failwith "Game.resolve_pending_inits: Impossible case" in
  let objs, cmdss = List.unzip (aux state.objs state.pending_inits) in
  ({ state with objs; pending_inits = [] }, cmdss)

let dispatch_messages env cmdss state =
  let rec aux msgs objs cmdss = match msgs, objs, cmdss with
    | _, [], [] -> []
    | [], _, _ -> List.map2_exn ~f:(fun obj cmds -> (obj, cmds)) objs cmdss
    | msg :: msgs', obj :: objs', cmds :: cmdss' ->
      if msg.receiver = Object.handle obj then
        Object.receive env msg.sender msg.data obj cmds
        :: aux msgs' objs' cmdss'
      else
        (obj, cmds) :: aux msgs objs' cmdss'
    | _, _, _ -> failwith "Game.dispatch_messages: Imposible case" in
  let message_compare msg1 msg2 =
    -(Env.Handle.compare msg1.receiver msg2.receiver) in
  let msgs = List.sort ~cmp:message_compare state.messages in
  let objs, cmdss = List.unzip (aux msgs state.objs cmdss) in
  ({ state with objs; messages = [] }, cmdss) 

let advance_sprites state =
  let objs = List.map ~f:(Object.advance_sprite state.time.t_ms) state.objs in
  { state with objs }

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

let react env events cmdss state =
  let objs, cmdss =
    List.unzip (List.map2_exn ~f:(Object.react env events) state.objs cmdss) in
  ({ state with objs }, cmdss)

let think env cmdss state =
  let objs, cmdss =
    List.unzip (List.map2_exn ~f:(Object.think env) state.objs cmdss) in
  ({ state with objs }, cmdss)

let process_command obj state cmd =
  let open Cmd in
  match cmd with
  | Message (data, receiver) ->
    let msg = { sender = Object.handle obj; receiver; data } in
    { state with messages = msg :: state.messages }
  | Print text -> print_endline text; state
  | Focus ->
    let pos = Body.pos (Object.body obj) in
    { state with view = View.focus (v_to_ints pos) state.view }

let process_commands cmds state =
  let aux state obj cmds = List.fold ~f:(process_command obj) ~init:state
                                     (List.rev cmds) in
  List.fold2_exn state.objs cmds ~f:aux ~init:state

let draw state =
  Canvas.clear Sdlvideo.gray;
  Room.draw state.view state.room;
  List.iter ~f:(Object.draw state.view) state.objs;
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

let rec loop state =
  let env = env_of_state state in
  let state, cmdss = resolve_pending_inits env state in
  (* Advancing sprites here may seem out of place. But this is done in order
   * to prevent freshly set animations to be affected. *)
  let state = advance_sprites state in
  let state, cmdss = dispatch_messages env cmdss state in
  let state, obj_events = process_events state in
  let state, cmdss = react env obj_events cmdss state in
  let state, cmdss = think env cmdss state in
  let state = process_commands cmdss state in
  draw state;
  let state = update_time state in
  loop state

let main () =
  at_exit quit;
  let save_filename = filename_concat ["data"; "saves"; "new.save"] in
  let save = save_of_sexp (Sexp.load_sexp save_filename) in
  loop (init ~save:save)

let () = main ()