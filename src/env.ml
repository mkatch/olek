open Core.Std
open Utils

module Handle = struct
  type t = int with sexp
  let compare = Int.compare
end

module HandleMap = Map.Make(Handle)

type handle = Handle.t

type t = {
  t : float;
  dt : float;
  context : Context.t;
  tiles : Tile.t Grid.t;
  named_bodies : (handle * Body.t) StringMap.t;
  bodies : Body.t HandleMap.t;
}

let t env = env.t
let dt env = env.dt
let context env = env.context

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
  }

let next_handle = ref 0 
let new_handle () =
  let handle = !next_handle in
  next_handle := !next_handle + 1;
  handle

let tile_coords_at (x, y) =
  let aux z =
    if z < 0.
    then Int.of_float z / Tile.size - 1
    else Int.of_float z / Tile.size in
  (aux y, aux x)

let tile_at pos env =
  let (i, j) = tile_coords_at pos in
  if Grid.are_coords_valid i j env.tiles
  then Grid.get i j env.tiles
  else Tile.Void

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

let handle env name = fst (StringMap.find_exn env.named_bodies name)

let body env handle = HandleMap.find_exn env.bodies handle

let named_body env name = snd (StringMap.find_exn env.named_bodies name)

(* This is so freakin insane! *)
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
