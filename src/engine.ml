open Core.Std
open Utils
open Sdlkey
open Printf

include Engine_sub

type state = {
  screen : Sdlvideo.surface;
  objects : Object.t list
}

let rec make_objects n mind =
  if n <= 0 then []
  else 
    Object.create (Vector.make (Random.float 800.) (Random.float 600.)) mind
    :: make_objects (n - 1) mind

let create () =
  Sdl.init [`VIDEO];
  let screen = Sdlvideo.set_video_mode ~w:800 ~h:600 [] in
  let square = (module Square : Object.Mind)
  in {
    screen;
    objects = make_objects 1000 square
  }

let finalize () =
  Sdl.quit ()

let clear screen =
  let c = Sdlvideo.(map_RGB screen Sdlvideo.white)
  in Sdlvideo.fill_rect screen c

let think_all objs = List.map ~f:Object.think objs

let draw_all screen bodies =
  clear screen;
  let c = Sdlvideo.(map_RGB screen black) in
  let draw b =
    let r = Body.to_sdl_rect b
    in Sdlvideo.fill_rect ~rect:r screen c
  in List.iter ~f:draw bodies

let iter state =
  let rec process_events state =
    match Sdlevent.poll () with
    | None -> Some state
    | Some event -> Sdlevent.(match event with
      | QUIT
      | KEYDOWN { keysym = KEY_ESCAPE } -> None
      | _ -> process_events state
    )
  in match process_events state with
  | None -> None
  | Some state ->
    let objects' = think_all state.objects in
    draw_all state.screen (List.map ~f:Object.body objects');
    Sdlvideo.flip state.screen;
    Some { state with objects = objects' }