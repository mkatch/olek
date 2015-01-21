open Core.Std
open Utils
open Sdlevent
open Sdlkey

type state = {
  room : Room.t;
  view : View.t;
  active_tileset_tile : int
}

let window_width = 800
let window_height = 600

let init () =
  Sdl.init [`VIDEO];
  Sdlttf.init ();
  Canvas.init ~w:window_width ~h:window_height;
  Sdlkey.enable_unicode true;
  Sdlkey.enable_key_repeat ();
  {
    view = View.make Vector.nil;
    room = Room.add_layer (Room.Uniform Sdlvideo.blue) (Room.make 16 16);
    active_tileset_tile = 0;
  }

let quit () =
  Sdl.quit ();
  Sdlttf.quit ()

let draw state =
  let view = state.view in
  let active_tileset_tile = state.active_tileset_tile in
  Canvas.clear Sdlvideo.blue;
  Room.draw state.room view;
  Tileset.draw (Room.tileset state.room) ~x:0 ~y:0 ~active:active_tileset_tile;
  Canvas.flip ()

let move_active_tileset_tile di dj state =
  let k = state.active_tileset_tile in
  let i = k / Tileset.column_cnt + di in
  let j = k mod Tileset.column_cnt + dj in
  let i = clamp ~min:0 ~max:(Tileset.row_cnt - 1) i in
  let j = clamp ~min:0 ~max:(Tileset.column_cnt - 1) j in
  { state with active_tileset_tile = i * Tileset.column_cnt + j }

let rec loop ?redraw:(redraw = true) state =
  if redraw then draw state;
  match wait_event () with
  | QUIT
  | KEYDOWN { keysym = KEY_ESCAPE } -> exit 0
  | KEYDOWN { keysym = KEY_RETURN } -> (
    match Terminal.read () with
    | None -> print_endline "escaped"; loop state
    | Some text -> print_endline ("text: " ^ text); loop state )
  | KEYDOWN { keysym = KEY_w } -> loop (move_active_tileset_tile (-1) 0 state)
  | KEYDOWN { keysym = KEY_a } -> loop (move_active_tileset_tile 0 (-1) state)
  | KEYDOWN { keysym = KEY_s } -> loop (move_active_tileset_tile 1 0 state)
  | KEYDOWN { keysym = KEY_d } -> loop (move_active_tileset_tile 0 1 state)
  | _ -> loop state ~redraw:false

let main () =
  at_exit quit;
  loop (init ())

let () = main ()