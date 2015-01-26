open Core.Std
open Utils

type state = {
  name : string;
  room : Room.t;
  view : View.t;
  active_layer : int;
  active_tileset_tile : int;
  hover_tile : int * int;
  tiles_pos : int;
  mode : [`World | `Obj];
  dragging : [`DraggingWorld | `DraggingObj | `DraggingImage | `NoDragging];
  draw_tiles : bool;
  draw_exclusive : bool;
}

let window_width = 800
let window_height = 600

let make name room =
  let w, h = Room.dims_px room in
  {
    name = name;
    room = room;
    view = View.make ((w, h) /^ 2);
    active_layer = 0;
    active_tileset_tile = 0;
    hover_tile = (0, 0);
    tiles_pos = 0;
    mode = `World;
    dragging = `NoDragging;
    draw_tiles = true;
    draw_exclusive = false;
  }

let update_caption state =
  Sdlwm.set_caption ~title:("Olek Editor: " ^ state.name) ~icon:""

let init () =
  Sdl.init [`VIDEO];
  Sdlttf.init ();
  Canvas.init ~w:window_width ~h:window_height;
  Sdlkey.enable_unicode true;
  Sdlkey.enable_key_repeat ();
  let state = make "untitled" (Room.make 16 16) in
  update_caption state; state

let quit () =
  Sdl.quit ();
  Sdlttf.quit ()

let map_editor_to_room state i =
  if i = state.tiles_pos then -1 else
  if i > state.tiles_pos then i - 1
  else i

let map_room_to_editor state i =
  if i = (-1) then state.tiles_pos else
  if i >= state.tiles_pos then i + 1
  else i

let tileset state =
  if state.active_layer = state.tiles_pos then Tileset.tiles
  else Room.tileset state.room

let draw_tileset state =
  let tileset = tileset state in
  let active = state.active_tileset_tile in
  Tileset.draw tileset ~pos:(0, 0) ~active:active

let layer_item_margin = 5
let layer_item_height = 17 + 2 * layer_item_margin
let draw_layer_list state =
  let layer_cnt = Room.layer_cnt state.room + 1 in
  let w = Tileset.width in
  let y0 = Tileset.height + layer_cnt * layer_item_height in
  let margin = layer_item_margin in
  let dy = layer_item_height in 
  let draw_item i kind =
    let y = y0 - dy * (i + 1) in
    let text = Int.to_string i ^ ": " ^ kind in
    let fg, bg =
      if i = state.active_layer then (Sdlvideo.white, Sdlvideo.blue)
      else (Sdlvideo.black, Sdlvideo.gray) in
    Canvas.draw_filled_rect bg (Sdlvideo.rect ~x:0 ~y:y ~w:w ~h:dy);
    Canvas.draw_text (margin, y + margin) fg text in
  let aux i layer =
    let i = map_room_to_editor state i in
    let kind = match layer with
      | Room.Uniform _ -> "uniform"
      | Room.Image _ -> "image"
      | Room.Tiled _ -> "tiled" in
    draw_item i kind in
  List.iteri ~f:aux (Room.layers state.room);
  draw_item state.tiles_pos "physical *"

let draw_hud state =
  let layer_cnt = Room.layer_cnt state.room + 1 in
  let height = Tileset.height + layer_cnt * layer_item_height in
  let rect = Sdlvideo.rect 0 0 Tileset.width height in
  let rect = Sdlvideo.inflate_rect 2 rect in
  Canvas.draw_rect Sdlvideo.black rect;
  draw_layer_list state;
  draw_tileset state

let draw_stamp state =
  if state.mode = `World && Sdlkey.(get_mod_state () land kmod_shift) = 0 then
  let l = map_editor_to_room state state.active_layer in
  if Room.layer_is_tiled state.room l then
  let tileset = tileset state in
  let active = state.active_tileset_tile in
  let (i, j) = state.hover_tile in
  let hover_tile_pos = Tile.size *^ (j, i) in
  let src_rect = Tileset.tile_rect tileset active in
  let src = Tileset.surface tileset in
  View.blit state.view ~pos:hover_tile_pos ~src_rect:src_rect src

let draw state =
  let only =
    if state.draw_exclusive then map_editor_to_room state state.active_layer
    else (-2) in 
  Canvas.clear Sdlvideo.gray;
  Room.draw state.view state.room ~draw_frame:true
    ~draw_stubs:(not state.draw_exclusive)
    ~draw_tiles:(state.mode = `World && state.draw_tiles)
    ~draw_stub_frames:(state.mode = `Obj)
    ~only:only;
  draw_stamp state;
  draw_hud state;
  Canvas.flip ()

let move_active_tileset_tile di dj state =
  let k = state.active_tileset_tile in
  let i = k / Tileset.column_cnt + di in
  let j = k mod Tileset.column_cnt + dj in
  let i = clamp ~min:0 ~max:(Tileset.row_cnt - 1) i in
  let j = clamp ~min:0 ~max:(Tileset.column_cnt - 1) j in
  { state with active_tileset_tile = i * Tileset.column_cnt + j }

let change_active_layer di state =
  let max_layer = Room.layer_cnt state.room  in
  let active_layer = clamp ~min: 0 ~max:max_layer (state.active_layer + di) in
  { state with active_layer }

let move_active_layer di state =
  let layer_cnt = Room.layer_cnt state.room + 1 in
  let active_layer = clamp ~min:0 ~max:(layer_cnt - 1)
    (state.active_layer + di) in
  if state.active_layer = state.tiles_pos then
    { state with active_layer; tiles_pos = active_layer } else
  if active_layer = state.tiles_pos then
    (* We assume di is always either -1 or 1 *)
    { state with active_layer; tiles_pos = state.active_layer }
  else
    let dst = map_editor_to_room state active_layer in
    let src = map_editor_to_room state state.active_layer in
    let room = Room.move_layer ~src:src ~dst:dst state.room in 
    { state with active_layer; room }

let update_hover_tile x y state =
  let (w, h) = Room.dims state.room in
  let (x, y) = View.to_world state.view (x, y) in
  let i = clamp ~min:0 ~max:(h - 1) (y / Tile.size) in
  let j = clamp ~min:0 ~max:(w - 1) (x / Tile.size) in
  { state with hover_tile = (i, j) }

let put_tile state =
  if state.mode <> `World then state else
  let open Sdlkey in
  let l = map_editor_to_room state state.active_layer in
  if Room.layer_is_tiled state.room l then
    let (i, j) = state.hover_tile in
    let t =
      if get_mod_state () land kmod_shift = 0 then state.active_tileset_tile
      else -1 in
    let room = Room.put_tile i j ~layer:l ~tile:t state.room in
    { state with room } 
  else state

let new_re = Str.regexp
  " *new +\\([a-z]+\\) +\\([0-9]+\\) +\\([0-9]+\\) *$"
let new_action text state =
  let name = Str.matched_group 1 text in
  let column_cnt = Int.of_string (Str.matched_group 2 text) in
  let row_cnt = Int.of_string (Str.matched_group 3 text) in
  let filename = filename_concat ["data"; "rooms"; name ^ ".room"] in
  let proceed = Sys.file_exists filename = `No
             || Terminal.confirm ("Room '" ^ name ^ "' exists. Proceed?") in
  if not proceed then state
  else
    let state = make name (Room.make row_cnt column_cnt) in
    update_caption state;
    state

let mode_re = Str.regexp
  " *mode +\\(world\\|obj\\) *$"
let mode_action text state =
  match Str.matched_group 1 text with
  | "world" -> { state with mode = `World }
  | "obj" -> { state with mode = `Obj }
  | _ -> state

let save_re = Str.regexp
  " *save *\\([a-z]+\\)? *$"
let save_action text state =
  let name = try Str.matched_group 1 text with Not_found -> state.name in
  let filename = filename_concat ["data"; "rooms"; name ^ ".room"] in
  let do_save = Sys.file_exists filename = `No
             || Terminal.confirm ("Overwrite '" ^ name ^ "'?") in
  if not do_save then state else
  let state = { state with name } in
  Sexp.save_hum filename (Room.sexp_of_t state.room);
  update_caption state;
  Terminal.show ("Saved '" ^ name ^ "'");
  state

let load_re = Str.regexp
  " *load *\\( \\([a-z]+\\)\\)? *$"
let load_action text state =
  let name = Str.matched_group 2 text in
  let filename = filename_concat ["data"; "rooms"; name ^ ".room"] in
  try
    let room = Room.t_of_sexp (Sexp.load_sexp filename) in
    let state = make name room in
    update_caption state;
    state
  with _ -> Terminal.show_error ("Unable to load room '" ^ name ^ "'"); state

let show_hide_tiles_re = Str.regexp
  " *\\(show\\|hide\\) +tiles *$"
let show_hide_tiles_action text state =
  match Str.matched_group 1 text with
  | "show" -> { state with draw_tiles = true }
  | "hide" -> { state with draw_tiles = false }
  | _ -> failwith "Impossible case"

let set_tileset_re = Str.regexp
  " *set +tileset +\\([a-z]+\\) *$"
let set_tileset_action text state =
  let name = Str.matched_group 1 text in
  { state with room = Room.set_tileset name state.room }

let add_uniform_layer_re = Str.regexp
  " *add +uniform +layer +\\([0-9]+\\) +\\([0-9]+\\) +\\([0-9]+\\) *$"
let add_uniform_layer_action text state =
  let r = clamp ~min:0 ~max:255 (Int.of_string (Str.matched_group 1 text)) in
  let g = clamp ~min:0 ~max:255 (Int.of_string (Str.matched_group 2 text)) in
  let b = clamp ~min:0 ~max:255 (Int.of_string (Str.matched_group 3 text)) in
  let tiles_pos = state.tiles_pos + 1 in
  { state with room = Room.add_uniform_layer (r, g, b) state.room; tiles_pos }

let add_image_layer_re = Str.regexp
  " *add +image +layer +\\([a-z]+\\) *$"
let add_image_layer_action text state =
  let name = Str.matched_group 1 text in
  try { state with room = Room.add_image_layer name state.room }
  with _ -> Terminal.show_error ("No background named '" ^ name ^ "'"); state

let add_tiled_layer_re = Str.regexp
  " *add +tiled +layer *$"
let add_tiled_layer_action text state =
  let tiles_pos = state.tiles_pos + 1 in
  { state with room = Room.add_tiled_layer state.room; tiles_pos }

let rem_layer_re = Str.regexp
  " *rem +layer *$"
let rem_layer_action text state =
  if state.tiles_pos = state.active_layer then (
    Terminal.show_error "Cannot remove phsical layer";
    state )
  else
    if not (Terminal.confirm "Remove layer?") then state
    else
      let i = map_editor_to_room state state.active_layer in
      { state with room = Room.rem_layer i state.room }

let set_repeat_re = Str.regexp
  " *set +repeat +\\(-\\|x\\|y\\|xy\\) *$"
let set_repeat_action text state =
  let repeat =
    match Str.matched_group 1 text with
    | "-" -> (false, false)
    | "x" -> (true, false)
    | "y" -> (false, true)
    | "xy" -> (true, true)
    | _ -> failwith "Impossible case" in
  let l = map_editor_to_room state state.active_layer in
  try
    let room = Room.set_image_layer_repeat repeat l state.room in
    { state with room }
  with _ -> Terminal.show_error "Active layer is not an image"; state

let set_parallax_re = Str.regexp
  " *set +parallax +\\([0-9]+\\.[0-9]+\\) *$"
let set_parallax_action text state =
  let parallax = Float.of_string (Str.matched_group 1 text) in
  let l = map_editor_to_room state state.active_layer in
  try
    let room = Room.set_image_layer_parallax parallax l state.room in
    { state with room }
  with _ -> Terminal.show_error "Active layer is not an image"; state

let add_obj_re = Str.regexp
  " *add +obj +\\([a-z]+\\) *$"
let add_obj_action text state =
  let mind_name = Str.matched_group 1 text in
  match Mind.find mind_name with
  | None -> Terminal.show_error ("No mind named '" ^ mind_name ^ "'"); state
  | Some mind ->
    let (module M : Mind.MIND) = mind in
    let pos = View.center state.view in
    let init = M.sexp_of_init M.default_init in
    let stub = Object.make_stub ~name:None ~mind:mind ~pos:pos ~init:init in
    { state with room = Room.add_stub stub state.room }

let set_name_re = Str.regexp
  " *set +name *\\(-\\|[a-z]+\\) *$"
let set_name_action text state =
  let name = Str.matched_group 1 text in
  let name = if name = "-" then None else Some name in
  let room = Room.map_selected_stub ~f:(Object.set_stub_name name) state.room in
  { state with room }

let edit_init_re = Str.regexp
  " *edit +init *$"
let edit_init_action text state =
  if state.mode <> `Obj then (
    Terminal.show_error "Must be in 'obj' mode";
    state ) else
  if List.is_empty (Room.stubs state.room) then (
    Terminal.show_error "No objects";
    state )
  else
  let edit stub =
    let (module M : Mind.MIND) = Object.stub_mind stub in
    let rec aux init_string =
      match Terminal.read ~text:init_string () with
      | None -> stub
      | Some init_string ->
        try
          let init = Sexp.of_string init_string in
          ignore (M.init_of_sexp init);
          Object.set_stub_init init stub
        with _ ->
          Terminal.show_error "Incorrect expression";
          aux init_string in
    aux (Sexp.to_string (Object.stub_init stub)) in
  { state with room = Room.map_selected_stub ~f:edit state.room }

let rem_obj_re = Str.regexp
  " *rem +obj *$"
let rem_obj_action text state =
  if state.mode <> `Obj then (
    Terminal.show_error "Must be in 'obj' mode";
    state ) else
  if List.is_empty (Room.stubs state.room) then (
    Terminal.show_error "No objects";
    state ) else
  if not (Terminal.confirm "Remove object?") then state
  else { state with room = Room.rem_selected_stub state.room }

let actions = [
  mode_re,              mode_action;
  new_re,               new_action;
  save_re,              save_action;
  load_re,              load_action;
  show_hide_tiles_re,   show_hide_tiles_action;

  set_tileset_re,       set_tileset_action;
  add_uniform_layer_re, add_uniform_layer_action;
  add_image_layer_re,   add_image_layer_action;
  add_tiled_layer_re,   add_tiled_layer_action;
  rem_layer_re,         rem_layer_action;

  set_repeat_re,        set_repeat_action;
  set_parallax_re,      set_parallax_action;

  add_obj_re,           add_obj_action;
  set_name_re,          set_name_action;
  edit_init_re,         edit_init_action;
  rem_obj_re,           rem_obj_action;
]

let process_terminal_command text state =
  let rec aux = function
    | [] -> Terminal.show_error "Unknown command"; state
    | (re, action) :: actions ->
      if Str.string_match re text 0 then action text state
      else aux actions in
  aux actions

let rec loop ?redraw:(redraw = true) state =
  if redraw then draw state;
  let open Sdlevent in
  let open Sdlkey in
  let open Sdlmouse in
  match wait_event () with
  | QUIT
  | KEYDOWN { keysym = KEY_ESCAPE } -> exit 0
  | KEYDOWN { keysym = KEY_RETURN } -> (
    match Terminal.read () with
    | None -> loop state
    | Some text -> loop (process_terminal_command text state) )
  | KEYDOWN { keysym = KEY_w } -> loop (move_active_tileset_tile (-1) 0 state)
  | KEYDOWN { keysym = KEY_a } -> loop (move_active_tileset_tile 0 (-1) state)
  | KEYDOWN { keysym = KEY_s } -> loop (move_active_tileset_tile 1 0 state)
  | KEYDOWN { keysym = KEY_d } -> loop (move_active_tileset_tile 0 1 state)
  | KEYDOWN { keysym = KEY_DOWN; keymod; } ->
    if keymod land kmod_shift <> 0 then loop (move_active_layer (-1) state)
    else loop (change_active_layer (-1) state)
  | KEYDOWN { keysym = KEY_UP; keymod; } ->
    if keymod land kmod_shift <> 0 then loop (move_active_layer 1 state) 
    else loop (change_active_layer 1 state)
  | MOUSEMOTION { mme_x; mme_y; mme_xrel; mme_yrel; mme_state } -> (
    match state.dragging with
    | `DraggingObj ->
      let aux = Object.move_stub_by (mme_xrel, mme_yrel) in
      let room = Room.map_selected_stub ~f:aux state.room in
      loop { state with room }
    | `DraggingImage ->
      let l = map_editor_to_room state state.active_layer in
      let room = Room.move_image_layer_by (mme_xrel, mme_yrel) l state.room in
      loop { state with room }
    | `DraggingWorld ->
      loop { state with view = View.move_by (mme_xrel, mme_yrel) state.view }
    | `NoDragging ->
      let state = update_hover_tile mme_x mme_y state in
      if List.mem mme_state BUTTON_LEFT then loop (put_tile state)
      else loop state )
  | MOUSEBUTTONDOWN { mbe_button = BUTTON_LEFT; mbe_x; mbe_y } -> (
    match state.mode with
    | `Obj ->
      let cursor = View.to_world state.view (mbe_x, mbe_y) in
      let room, selected = Room.select_stub cursor state.room in
      let dragging =
        if is_key_pressed KEY_SPACE && not selected then `DraggingWorld else
        if selected then `DraggingObj
        else `NoDragging in
      loop { state with room; dragging }
    | `World ->
      let l = map_editor_to_room state state.active_layer in
      if is_key_pressed KEY_SPACE then
        loop { state with dragging = `DraggingWorld } ~redraw:false else
      if Room.layer_is_tiled state.room l then
        loop (put_tile state) else
      if Room.layer_is_image state.room l then
        loop { state with dragging = `DraggingImage } ~redraw:false
      else
        loop state ~redraw:false )
  | MOUSEBUTTONUP { mbe_button = BUTTON_LEFT }
  | KEYUP { keysym = KEY_SPACE } ->
    loop { state with dragging = `NoDragging } ~redraw:false
  | KEYDOWN { keysym = KEY_e } -> loop { state with draw_exclusive = true }
  | KEYUP { keysym = KEY_e } -> loop { state with draw_exclusive = false }
  | _ -> loop state ~redraw:false

let main () =
  at_exit quit;
  loop (init ())

let () = main ()