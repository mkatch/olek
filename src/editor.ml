open Core.Std
open Sdlevent

type state = {
  canvas : Canvas.t;
  redraw : bool;
}

let window_width = 800
let window_height = 600

let init () =
  Sdl.init [`VIDEO];
  let canvas = Canvas.init ~w:window_width ~h:window_height in
  {
    canvas = canvas;
    redraw = true;
  }

let quit () = Sdl.quit ()

let process_event state = function
  | QUIT -> None
  | _ -> Some state

let rec process_events state =
  match Sdlevent.poll () with
  | None -> Some state
  | Some event ->
    match process_event state event with
    | None -> None
    | Some state -> process_events state 

let draw state = if state.redraw then
  Canvas.clear state.canvas Sdlvideo.blue;
  Canvas.flip state.canvas

let main () =
  let rec loop state =
    match process_events state with
    | None -> quit ()
    | Some state ->
      draw state;
      loop state in
  loop (init ())

let () = main ()