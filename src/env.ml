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
  tiles : Tile.t Grid.t;
  named_bodies : (handle * Body.t) StringMap.t;
  bodies : Body.t HandleMap.t;
}

let t env = env.t

let dt env = env.dt

let make ~t_ms ~dt_ms ~room ~objs =
  let rec filter_named = function
    | [] -> []
    | (Some name, handle, body) :: objs ->
      (name, (handle, body)) :: filter_named objs
    | (None, _, _) :: objs -> filter_named objs in
  let take_handle_body (_, handle, body) = (handle, body) in
  {
    t = Float.of_int t_ms /. 1000.0;
    dt = Float.of_int dt_ms /. 1000.0;
    tiles = Room.tiles room;
    named_bodies = StringMap.of_alist_exn (filter_named objs);
    bodies = HandleMap.of_alist_exn (List.map ~f:take_handle_body objs);
  }

let next_handle = ref 0 
let new_handle () =
  let handle = !next_handle in
  next_handle := !next_handle + 1;
  handle

let tile_at pos env =
  let (i, j) = v_to_ints pos /^ 16 in
  Grid.get i j env.tiles

let handle env name = fst (StringMap.find_exn env.named_bodies name)

let body env handle = HandleMap.find_exn env.bodies handle

let named_body env name = snd (StringMap.find_exn env.named_bodies name)