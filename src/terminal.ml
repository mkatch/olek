open Core.Std
open Sdlevent
open Sdlkey

type t = {
  text : string;
  pos : int;
  command : string;
}

let empty = {
  text = "";
  pos = 0;
  command = "";
}

let text terminal = terminal.text

let pos terminal = terminal.pos

let command terminal = terminal.command

let process_keydown_event event terminal =
  let text = terminal.text in
  let pos = terminal.pos in
  let length = String.length text in
  match event.keysym with
  | KEY_RETURN -> { text = ""; pos = 0; command = text }
  | KEY_BACKSPACE ->
    let pos = max 0 (pos - 1) in
    let text = String.prefix text pos ^ String.suffix text (length - pos - 1) in
    { terminal with text; pos; }
  | KEY_LEFT -> { terminal with pos = max 0 (pos - 1) }
  | KEY_RIGHT -> { terminal with pos = min length (pos + 1) }
  | _ ->
  match event.keycode with
  | 'a'..'z'|'A'..'Z'|'0'..'9'|' '|'.' as code ->
    let c = String.make 1 code in
    let text = String.prefix text pos ^ c ^ String.suffix text (length - pos) in
    { terminal with text; pos = pos + 1}
  | _ -> terminal