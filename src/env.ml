open Core.Std
open Utils

module Handle = struct
  type t = int with sexp (* Sexp is needed by Map *)
  let nil = -1
  let compare = Int.compare
end

module HandleMap = Map.Make(Handle)

type handle = Handle.t

type user = {
  handle : handle;
  name : string option;
}

type t = {
  t : float;
  dt : float;
  context : Context.t;
  tiles : Tile.t Grid.t;
  named_bodies : (handle * Body.t) StringMap.t;
  bodies : Body.t HandleMap.t;
  user : user;
}

let make ~t_ms ~dt_ms ~context ~tiles ~objs =
  let rec filter_named = function
    | [] -> []
    | (Some name, handle, body) :: objs ->
      (name, (handle, body)) :: filter_named objs
    | (None, _, _) :: objs -> filter_named objs in
  let take_handle_body (_, handle, body) = (handle, body) in
  {
    t = Float.of_int t_ms /. 1000.0;
    dt = Float.of_int dt_ms /. 1000.0;
    context = context;
    tiles = tiles;
    named_bodies = StringMap.of_alist_exn (filter_named objs);
    bodies = HandleMap.of_alist_exn (List.map ~f:take_handle_body objs);
    user = { handle = Handle.nil; name = None; }
  }

let t env = env.t
let dt env = env.dt
let context env = env.context

let set_user handle name env = { env with user = { handle; name; } }
let my_handle env = env.user.handle
let my_name env = env.user.name
let my_name_exn env = Option.value_exn env.user.name

let next_handle = ref 0 
let new_handle () =
  let handle = !next_handle in
  next_handle := !next_handle + 1;
  handle

let handle env name = fst (StringMap.find_exn env.named_bodies name)
let body env handle = HandleMap.find_exn env.bodies handle
let named_body env name = snd (StringMap.find_exn env.named_bodies name)

let to_grid z = Int.of_float (Float.round_down (z /. Float.of_int Tile.size))
let to_grid_inclusive z =
  Int.of_float (Float.round_up (z /. Float.of_int Tile.size)) - 1
let to_grid2 (x, y) = (to_grid y, to_grid x)
let to_grid2_inclusive (x, y) = (to_grid_inclusive y, to_grid_inclusive x)

let tile (i, j) env = Grid.get_safe i j env.tiles ~default:Tile.Void
let tile_at pos env = tile (to_grid2 pos) env
let tile_at_inclusive pos env = tile (to_grid2 pos) env

let has_foundation env body =
  let (l, b, r) = Body.(l body, b body, r body) in
  let (j0, i, j1) = (to_grid l, to_grid b, to_grid r) in
  let js = List.range ~stop:`inclusive j0 j1 in
  list_any ~f:(fun j -> Tile.is_t_solid (tile (i, j) env)) js

let leaning_left env body =
  let (t, l, b) = Body.(t body, l body, b body) in
  let (i0, j, i1) = (to_grid t, to_grid_inclusive l, to_grid_inclusive b) in
  let is = List.range ~stop:`inclusive i0 i1 in
  list_any ~f:(fun i -> Tile.is_r_solid (tile (i, j) env)) is

let leaning_right env body =
  let (t, r, b) = Body.(t body, r body, b body) in
  let (i0, j, i1) = (to_grid t, to_grid r, to_grid_inclusive b) in
  let is = List.range ~stop:`inclusive i0 i1 in
  list_any ~f:(fun i -> Tile.is_l_solid (tile (i, j) env)) is

let penetr_left l =
  let j = to_grid_inclusive l in
  let r = Float.of_int ((j + 1) * Tile.size) in
  r -. l

let penetr_right r =
  let j = to_grid r in
  let l = Float.of_int (j * Tile.size) in
  r -. l

(* This is not perfect but works and I have no time for this stuff any more! *)
let l_of_x x =
  if x < 0. 
  then Float.of_int ((Int.of_float x / Tile.size - 1) * Tile.size)
  else Float.of_int ((Int.of_float x / Tile.size) * Tile.size)
let r_of_x x = l_of_x x +. Float.of_int Tile.size
let t_of_y = l_of_x
let b_of_y = r_of_x
let penetr_l x = x -. l_of_x x
let penetr_t y = y -. t_of_y y
let penetr_r x = r_of_x x -. x
let penetr_b y = b_of_y y -. y
let collide env body ((vel_x, vel_y) as vel) =
  let (l, t, r, b) = Body.(l body, t body, r body, b body) in
  let (pl, pt, pr, pb) = (penetr_r l, penetr_b t, penetr_l r, penetr_t b) in
  let lt_tile = tile_at (l, t) env in
  let rt_tile = tile_at (r, t) env in
  let rb_tile = tile_at (r, b) env in
  let lb_tile = tile_at (l, b) env in
  let plt = (-.pl, -.pt) and prt = (pr, -.pt)
  and plb = (-.pl,   pb) and prb = (pr,   pb) in 
  let l_penetr =
    if vel_x >= 0. then 0. else
    if Tile.is_r_solid lb_tile && Tile.is_r_solid lt_tile
    || Tile.is_r_solid lb_tile && per vel plb >= 0.
    || Tile.is_r_solid lt_tile && per vel plt <  0. then pl
    else 0.
  and t_penetr =
    if vel_y >= 0. then 0. else
    if Tile.is_b_solid lt_tile && Tile.is_b_solid rt_tile
    || Tile.is_b_solid lt_tile && per vel plt >= 0.
    || Tile.is_b_solid rt_tile && per vel prt <  0. then pt
    else 0.
  and r_penetr =
    if vel_x <= 0. then 0. else
    if Tile.is_l_solid rt_tile && Tile.is_l_solid rb_tile
    || Tile.is_l_solid rt_tile && per vel prt >= 0.
    || Tile.is_l_solid rb_tile && per vel prb <  0. then pr
    else 0.
  and b_penetr =
    if vel_y <= 0. then 0. else
    if Tile.is_t_solid rb_tile && Tile.is_t_solid lb_tile
    || Tile.is_t_solid rb_tile && per vel prb >= 0.
    || Tile.is_t_solid lb_tile && per vel plb <  0. then pb
    else 0. in
  (l_penetr, t_penetr, r_penetr, b_penetr) 