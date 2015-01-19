open Core.Std
open Utils
open Sdlevent
open Sdlkey

type state = {
  canvas : Canvas.t;
  terminal : Terminal.t;
  redraw : bool;
  terminal_focus : bool;
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
    terminal = Terminal.empty;
    redraw = true;
    terminal_focus = false;
  }

let quit () = Sdl.quit ()

let process_keydown_event state event =
  if state.terminal_focus then
    let terminal = Terminal.process_keydown_event event state.terminal in
    if event.keysym = KEY_RETURN then
      Some { state with terminal; redraw = true; terminal_focus = false; }
    else
      Some { state with terminal; redraw = true; }
  else
    match event.keysym with
    | KEY_ESCAPE -> None
    | KEY_RETURN -> Some { state with terminal_focus = true }
    | _ -> Some state

let rec process_events state =
  match wait_event () with
  | QUIT -> None
  | KEYDOWN event -> process_keydown_event state event 
  | _ -> Some state

let draw state = if state.redraw then
  let c = state.canvas in
  Canvas.clear c Sdlvideo.blue;
  Canvas.draw_terminal c state.terminal;
  Canvas.flip c

let main () =
  let rec loop state =
    match process_events state with
    | None -> quit ()
    | Some state ->
      draw state;
      loop { state with redraw = false } in
  loop (init ())

let () = main ()