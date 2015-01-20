open Core.Std
open Utils
open Sdlevent
open Sdlkey

type state = {
  canvas : Canvas.t;
  room : Room.t;
}

let window_width = 800
let window_height = 600

let init () =
  Sdl.init [`VIDEO];
  Sdlkey.enable_unicode true;
  Sdlkey.enable_key_repeat ();
  Sdlttf.init ();
  let canvas = Canvas.init ~w:window_width ~h:window_height in
  {
    canvas = canvas;
    room = Room.add_layer (Room.Uniform (255, 255, 0)) (Room.make 16 16);
  }

let quit () = Sdl.quit ()

let draw state =
  let c = state.canvas in
  Canvas.clear c Sdlvideo.blue;
  Canvas.draw_room c state.room;
  Canvas.flip c

let rec loop ?redraw:(redraw = true) state =
  if redraw then draw state;
  match wait_event () with
  | QUIT
  | KEYDOWN { keysym = KEY_ESCAPE } -> exit 0
  | KEYDOWN { keysym = KEY_RETURN } -> (
    match Terminal.read () with
    | None -> print_endline "escaped"; loop state
    | Some text -> print_endline ("text: " ^ text); loop state )
  | _ -> loop state ~redraw:false

let main () =
  at_exit quit;
  loop (init ())

let () = main ()