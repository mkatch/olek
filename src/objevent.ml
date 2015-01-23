open Sdlevent
open Sdlkey

type t =
  | KeyDown of Sdlkey.t
  | KeyUp of Sdlkey.t

let of_sdl_event = function
  | KEYDOWN k -> [KeyDown k.keysym]
  | KEYUP k -> [KeyUp k.keysym]
  | _ -> []