open Core.Std
open Utils

type sheet = {
  surface : Sdlvideo.surface;
  frames : int;
  dt : int;
  origin : int * int;
}

type t = {
  sheet : sheet;
  frame : int;
  t : int;
}

let make_sheet ~image ~frames ~dt ~origin =
  let surface = Sdlloader.load_image ("data/sprites/" ^ image ^ ".png") in
  { surface; frames; dt; origin; }

let make sheet = {
  sheet = sheet;
  frame = 0;
  t = Sdltimer.get_ticks ();
}

let sheet sprite = sprite.sheet

let sheet_dims sheet =
   let w, h, _ = Sdlvideo.surface_dims sheet.surface in
   (w, h)

let frame_dims sheet =
   let w, h, _ = Sdlvideo.surface_dims sheet.surface in
   (w / sheet.frames, h)

let origin sheet = sheet.origin

let advance t sprite =
  let frames = sprite.sheet.frames in
  if frames <= 1 then sprite else
  let prev_t = sprite.t in
  let dt = sprite.sheet.dt in
  let dframe = (t - prev_t) / dt in
  if (dframe <= 0) then sprite else
  let frame = (sprite.frame + dframe) mod frames in
  { sprite with frame; t }

let draw view pos sprite =
  let pos = pos -^ sprite.sheet.origin in
  let w, h = frame_dims sprite.sheet in
  let src_rect = Sdlvideo.rect ~x:(sprite.frame * w) ~y:0 ~w:w ~h:h in
  View.blit view ~pos:pos ~src_rect:src_rect sprite.sheet.surface

let dummy_sheet = make_sheet ~image:"dummy" ~frames:1 ~dt:0 ~origin:(0, 0)

let dummy = make dummy_sheet