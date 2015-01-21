open Core.Std
open Utils
open Sdlevent
open Sdlkey

type state = {
  room : Room.t;
  view : View.t;
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
  }

let quit () = Sdl.quit ()

let draw state =
  let view = state.view in
  Canvas.clear Sdlvideo.blue;
  Canvas.draw_room view state.room;
  Canvas.flip ()

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