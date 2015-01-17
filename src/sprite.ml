open Core.Std
open Utils

type sheet = {
  surface : Sdlvideo.surface;
  frames : int;
  dt : int;
}

type instance = {
  sheet : sheet;
  frame : int;
  t : int;
}

let make_sheet ~image ~frames ~dt =
  let surface = Sdlloader.load_image ("data/sprites/" ^ image ^ ".png") in
  { surface; frames; dt }

let make_instance sheet = {
  sheet = sheet;
  frame = 0;
  t = Sdltimer.get_ticks ();
}

let sheet_dims sheet =
    let w, h, _ = Sdlvideo.surface_dims sheet.surface in (w, h)

let dims instance =
  let w, h = sheet_dims instance.sheet in
  (w / instance.sheet.frames, h)

let advance t instance =
  let frames = instance.sheet.frames in
  if frames <= 1 then instance else
  let prev_t = instance.t in
  let dt = instance.sheet.dt in
  let dframe = (t - prev_t) / dt in
  if (dframe <= 0) then instance else
  let frame = (instance.frame + dframe) mod frames in
  { instance with frame; t }

let blit_data instance =
  let w, h = dims instance in (
    instance.sheet.surface,
    Sdlvideo.rect ~x:(instance.frame * w) ~y:0 ~w:w ~h:h
  )

let dummy_sheet = make_sheet ~image:"dummy" ~frames:1 ~dt:0

let dummy_instance = make_instance dummy_sheet