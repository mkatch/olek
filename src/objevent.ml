open Sdlevent
open Sdlkey

type t =
  | NextFrame
  | KeyDown of Sdlkey.t
  | KeyUp of Sdlkey.t

let of_sdl_event = function
  | KEYDOWN k -> [KeyDown k.keysym]
  | KEYUP k -> [KeyUp k.keysym]
  | _ -> []

let is_next_frame = function
  | NextFrame -> true
  | _ -> false