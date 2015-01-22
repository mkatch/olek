open Core.Std
open Utils
open Sdlevent
open Sdlkey

type state = {
  room : Room.t;
  view : View.t;
  active_layer : int;
  active_tileset_tile : int;
  hover_tile : int * int;
  tiles_pos : int;
}

let window_width = 800
let window_height = 600

let init () =
  Sdl.init [`VIDEO];
  Sdlttf.init ();
  Canvas.init ~w:window_width ~h:window_height;
  Sdlkey.enable_unicode true;
  Sdlkey.enable_key_repeat ();
  {
    room = Room.make 16 16;
    view = View.make Vector.nil;
    active_layer = 0;
    active_tileset_tile = 0;
    hover_tile = (0, 0);
    tiles_pos = 0;
  }

let quit () =
  Sdl.quit ();
  Sdlttf.quit ()

let map_layer_to_room state i = if i >= state.tiles_pos then i + 1 else i

let draw_layer_list state =
  let w = Tileset.column_cnt * Tile.size in
  let y0 = Tileset.row_cnt * Tile.size in
  let margin = 5 in
  let dy = 19 + 2 * margin in 
  let draw_item i kind =
    let y = y0 + dy * i in
    let text = Int.to_string i ^ ": " ^ kind in
    let fg, bg =
      if i = state.active_layer then (Sdlvideo.white, Sdlvideo.blue)
      else (Sdlvideo.black, Sdlvideo.gray) in
    Canvas.draw_filled_rect (Sdlvideo.rect ~x:0 ~y:y ~w:w ~h:dy) bg;
    Canvas.draw_text margin (y + margin) ~fg:fg ~bg:bg text in
  let aux i layer =
    let i = map_layer_to_room state i in
    let kind = match layer with
      | Room.Uniform _ -> "uniform"
      | Room.Tiled _ -> "tiled" in
    draw_item i kind in
  List.iteri ~f:aux (Room.layers state.room);
  draw_item state.tiles_pos "physical *"

let draw state =
  let view = state.view in
  let tileset = Room.tileset state.room in
  let active_tileset_tile = state.active_tileset_tile in
  let (i, j) = state.hover_tile in
  let hover_tile_x = j * Tile.size and hover_tile_y = i * Tile.size in
  let tile_rect = Tileset.tile_rect tileset active_tileset_tile in
  Canvas.clear Sdlvideo.gray;
  Room.draw state.room view ~draw_tiles:true;
  Canvas.blit ~x:hover_tile_x ~y:hover_tile_y ~src_rect:tile_rect
    (Tileset.surface tileset);
  Tileset.draw (Room.tileset state.room) ~x:0 ~y:0 ~active:active_tileset_tile;
  draw_layer_list state;
  Canvas.flip ()

let move_active_tileset_tile di dj state =
  let k = state.active_tileset_tile in
  let i = k / Tileset.column_cnt + di in
  let j = k mod Tileset.column_cnt + dj in
  let i = clamp ~min:0 ~max:(Tileset.row_cnt - 1) i in
  let j = clamp ~min:0 ~max:(Tileset.column_cnt - 1) j in
  { state with active_tileset_tile = i * Tileset.column_cnt + j }

let change_active_layer di state =
  let layer_cnt = Room.layer_cnt state.room + 1 in
  let active_layer = (state.active_layer + di) mod layer_cnt in
  { state with active_layer }

let move_active_layer di state =
  let layer_cnt = Room.layer_cnt state.room + 1 in
  let active_layer = clamp ~min:0 ~max:(layer_cnt - 1)
    (state.active_layer + di) in
  if state.active_layer = state.tiles_pos then
    { state with active_layer; tiles_pos = active_layer }
  else
    let dst = map_layer_to_room state active_layer in
    let src = map_layer_to_room state state.active_layer in
    let room = Room.move_layer ~src:src ~dst:dst state.room in
    let tiles_pos = (* We assume di is always either -1 or 1 *)
      if state.tiles_pos = active_layer then state.active_layer
      else state.tiles_pos in 
    { state with active_layer; room; tiles_pos }

let update_hover_tile x y state =
  { state with hover_tile = (y / Tile.size, x / Tile.size)}

let set_tileset_re = Str.regexp
  " *set *tileset *\\([a-z]+\\) *$"
let set_tileset_action text state =
  let name = Str.matched_group 1 text in
  { state with room = Room.set_tileset name state.room }

let add_uniform_layer_re = Str.regexp
  " *add *uniform *layer *\\([0-9]+\\) *\\([0-9]+\\) *\\([0-9]+\\) *$"
let add_uniform_layer_action text state =
  let r = Int.of_string (Str.matched_group 1 text) in
  let g = Int.of_string (Str.matched_group 2 text) in
  let b = Int.of_string (Str.matched_group 3 text) in
  { state with room = Room.add_uniform_layer (r, g, b) state.room }

let add_tiled_layer_re = Str.regexp
  " *add *tiled *layer *$"
let add_tiled_layer_action text state =
  { state with room = Room.add_tiled_layer state.room }

let actions = [
  set_tileset_re,       set_tileset_action;
  add_uniform_layer_re, add_uniform_layer_action;
  add_tiled_layer_re,   add_tiled_layer_action;
]

let process_terminal_command text state =
  let rec aux = function
    | [] -> state
    | (re, action) :: actions ->
      if Str.string_match re text 0 then action text state
      else aux actions in
  aux actions

let rec loop ?redraw:(redraw = true) state =
  if redraw then draw state;
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
    if keymod land kmod_shift <> 0 then loop (move_active_layer 1 state)
    else loop (change_active_layer 1 state)
  | KEYDOWN { keysym = KEY_UP; keymod; } ->
    if keymod land kmod_shift <> 0 then loop (move_active_layer (-1) state) 
    else loop (change_active_layer (-1) state)
  | MOUSEMOTION { mme_x = x; mme_y = y } -> loop (update_hover_tile x y state)
  | _ -> loop state ~redraw:false

let main () =
  at_exit quit;
  loop (init ())

let () = main ()