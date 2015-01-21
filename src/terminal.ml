open Core.Std
open Sdlevent
open Sdlkey

let ta_h = 28
let marg = 5

type state = {
  prompt : string;
  text : string;
  pos : int;
  screenshot : Sdlvideo.surface;
}

let draw state =
  let text = state.text in
  let pos = state.pos in
  let length = String.length text in
  let text = String.prefix text pos ^ "|" ^ String.suffix text (length - pos) in
  let w, h = Canvas.dims () in
  Canvas.blit state.screenshot;
  Canvas.draw_filled_rect (Sdlvideo.rect 0 (h - ta_h) w ta_h) Sdlvideo.black;
  Canvas.draw_text marg (h - ta_h + marg) ~fg:Sdlvideo.white ~bg:Sdlvideo.black
    (state.prompt ^ " " ^ text);
  Canvas.flip ()

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
    screenshot = Canvas.screenshot ()
  } in
  loop state
