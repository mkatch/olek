open Core.Std
open Sdlevent
open Sdlkey

type state = {
  prompt : string;
  text : string;
  pos : int
}

let draw state =
  let text = state.text in
  let pos = state.pos in
  let length = String.length text in
  let text = String.prefix text pos ^ "|" ^ String.suffix text (length - pos) in
  Printf.printf "%s %s\n" state.prompt text; flush_all ()

let rec loop ?redraw:(redraw = true) state =
  if redraw then draw state;
  let pos = state.pos in
  let text = state.text in
  let length = String.length text in
  match wait_event () with
  | QUIT -> exit 0
  | KEYDOWN { keysym = KEY_ESCAPE } -> None
  | KEYDOWN { keysym = KEY_RETURN } -> Some state.text
  | KEYDOWN { keysym = KEY_BACKSPACE } ->
    let pos = max 0 (pos - 1) in
    let text = String.prefix text pos ^ String.suffix text (length - pos - 1) in
    loop { state with text; pos; }
  | KEYDOWN { keysym = KEY_LEFT } ->
    loop { state with pos = max 0 (pos - 1) }
  | KEYDOWN { keysym = KEY_RIGHT } ->
    loop { state with pos = min length (pos + 1) }
  | KEYDOWN { keycode = 'a'..'z'|'A'..'Z'|'0'..'9'|' '|'.' as code } ->
    let c = String.make 1 code in
    let text = String.prefix text pos ^ c ^ String.suffix text (length - pos) in
    loop { state with text; pos = pos + 1}
  | _ -> loop state ~redraw:false

let read ?prompt:(prompt = "> ") () =
  let state = {
    prompt = prompt;
    text = "";
    pos = 0;
  } in
  loop state
