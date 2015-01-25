open Core.Std
open Sdlevent
open Sdlkey

let ta_h = 28
let marg = 5

type state = {
  prompt : string;
  text : string;
  pos : int;
  is_error : bool;
  is_editable : bool;
  screenshot : Sdlvideo.surface;
}

let draw state =
  let text =
    if not state.is_editable then state.text
    else
      let text = state.text in
      let pos = state.pos in
      let length = String.length text in
      String.prefix text pos ^ "|" ^ String.suffix text (length - pos) in
  let w, h = Canvas.dims () in
  let fg = if state.is_error then Sdlvideo.red else Sdlvideo.white in
  Canvas.blit state.screenshot;
  Canvas.draw_filled_rect Sdlvideo.black (Sdlvideo.rect 0 (h - ta_h) w ta_h);
  Canvas.draw_text (marg, h - ta_h + marg) fg (state.prompt ^ " " ^ text);
  Canvas.flip ()

let rec loop ?redraw:(redraw = true) state =
  if redraw then draw state;
  let pos = state.pos in
  let text = state.text in
  let length = String.length text in
  let event = wait_event () in
  match event with
  | QUIT -> exit 0
  | KEYDOWN { keysym = KEY_ESCAPE } -> None
  | KEYDOWN { keysym = KEY_RETURN } -> Some state.text
  | _ -> if not state.is_editable then loop ~redraw:false state else
  match event with
  | KEYDOWN { keysym = KEY_BACKSPACE } ->
    if length = 0 then loop state else
    let pos = max 0 (pos - 1) in
    let text = String.prefix text pos ^ String.suffix text (length - pos - 1) in
    loop { state with text; pos; }
  | KEYDOWN { keysym = KEY_LEFT } ->
    loop { state with pos = max 0 (pos - 1) }
  | KEYDOWN { keysym = KEY_RIGHT } ->
    loop { state with pos = min length (pos + 1) }
  | KEYDOWN { keycode = 'a'..'z'|'A'..'Z'|'0'..'9'|' '|'.'|'-' as code } ->
    let c = String.make 1 code in
    let text = String.prefix text pos ^ c ^ String.suffix text (length - pos) in
    loop { state with text; pos = pos + 1}
  | _ -> loop state ~redraw:false

let read ?prompt:(prompt = ">") ?text:(text = "") () = loop {
  prompt = prompt;
  text = text;
  pos = 0;
  is_error = false;
  is_editable = true;
  screenshot = Canvas.screenshot ();
}

let show text = ignore (loop {
  prompt = "";
  text = text;
  pos = 0;
  is_error = false;
  is_editable = false;
  screenshot = Canvas.screenshot ();
})

let show_error text = ignore (loop {
  prompt = "Error:";
  text = text;
  pos = 0;
  is_error = true;
  is_editable = false;
  screenshot = Canvas.screenshot ();
})

let yes_re = Str.regexp " *[Yy]\\([Ee][Ss]?\\)? *$"
let confirm text =
  match
    loop {
      prompt = text ^ " (y/n):";
      text = "";
      pos = 0;
      is_error = false;
      is_editable = true;
      screenshot = Canvas.screenshot ();
    }
  with
  | None -> false
  | Some ans -> Str.string_match yes_re ans 0