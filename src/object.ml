open Core.Std
open Utils

type t = {
  handle : Env.handle;
  name : string option;
  body : Body.t;
  mind_instance : Mind.instance;
}

type stub = {
  obj_name : string option;
  mind : Mind.mind;
  pos : int * int;
  init : Sexp.t;
}
with sexp

let handle obj = obj.handle
let name obj = obj.name
let body obj = obj.body
let set_body body obj = { obj with body }

let make_stub ~name ~mind ~pos ~init = { obj_name = name; mind; pos; init; }

let make stub =
  let (module M : Mind.MIND) = stub.mind in
  let handle = Env.new_handle () in
  let name = stub.obj_name in
  let body = Body.set_pos (v_to_floats stub.pos) M.default_body in
  let state = M.default_state in
  let mind_instance = Mind.make_instance (module M) state in
  { handle; name; body; mind_instance; }

let init env stub obj =
  let env = Env.set_user obj.handle obj.name env in
  let (module I : Mind.INSTANCE) = obj.mind_instance in
  let init = I.Mind.init_of_sexp stub.init in
  let chain = I.Mind.init I.state obj.body env init in
  let state = Option.value (Cmd.get_state chain) ~default:I.state in
  let body = Option.value (Cmd.get_body chain) ~default:obj.body in
  let cmds = Cmd.get_cmds chain in
  let mind_instance = Mind.make_instance (module I.Mind) state in
  ({ obj with body; mind_instance }, cmds)

let think env obj cmds =
  let env = Env.set_user obj.handle obj.name env in
  let (module I : Mind.INSTANCE) = obj.mind_instance in
  let chain = I.Mind.think I.state obj.body env in
  let state = Option.value (Cmd.get_state chain) ~default:I.state in
  let body = Option.value (Cmd.get_body chain) ~default:obj.body in
  let mind_instance = Mind.make_instance (module I.Mind) state in
  let cmds = Cmd.get_cmds chain ~cmds:cmds in
  ({ obj with body; mind_instance }, cmds)

let react env events obj cmds =
  let env = Env.set_user obj.handle obj.name env in
  let (module I : Mind.INSTANCE) = obj.mind_instance in
  let rec aux body state cmds events =
    match events with
    | [] ->
      let mind_instance = Mind.make_instance (module I.Mind) state in
      ({ obj with body; mind_instance }, cmds)
    | event :: events ->
      let chain = I.Mind.react state body env event in
      let state = Option.value (Cmd.get_state chain) ~default:state in
      let body = Option.value (Cmd.get_body chain) ~default:obj.body in
      let cmds = Cmd.get_cmds chain ~cmds:cmds in
      aux body state cmds events in
  aux obj.body I.state cmds events

let receive env msgs obj cmds =
  let env = Env.set_user obj.handle obj.name env in
  let (module I : Mind.INSTANCE) = obj.mind_instance in
  let rec aux body state cmds msgs =
    match msgs with
    | [] ->
      let mind_instance = Mind.make_instance (module I.Mind) state in
      ({ obj with body; mind_instance }, cmds )
    | (sender, data) :: msgs ->
      let msg = I.Mind.msg_of_sexp data in
      let chain = I.Mind.receive state body env sender msg in
      let state = Option.value (Cmd.get_state chain) ~default:state in
      let body = Option.value (Cmd.get_body chain) ~default:body in
      let cmds = Cmd.get_cmds chain ~cmds:cmds in
      aux body state cmds msgs in
  aux obj.body I.state cmds msgs

let advance_sprite t obj = { obj with body = Body.advance_sprite t obj.body }

let draw view obj = Body.draw view obj.body

let stub_init stub = stub.init
let stub_mind stub = stub.mind

let stub_contains (x, y) stub =
  let (module M : Mind.MIND) = stub.mind in
  let w, h = Body.dims M.default_body in
  let (x0, y0) = stub.pos in
  2 * abs (x - x0) <= w && 2 * abs (y - y0) <= h 

let set_stub_name name stub = { stub with obj_name = name }
let set_stub_init init stub = { stub with init }
let move_stub_by dpos stub = { stub with pos = stub.pos +^ dpos }

let draw_stub view ?draw_frame:(draw_frame = false)
  ?frame_color:(frame_color = Sdlvideo.red) stub =
  let (module M : Mind.MIND) = stub.mind in
  let body = Body.set_pos (v_to_floats stub.pos) M.default_body in
  Body.draw view body;
  if draw_frame then (
  View.draw_rect view frame_color (Body.rect body);
  match stub.obj_name with
  | None -> ()
  | Some name ->
    let name_w, _ = Canvas.size_text name in
    let name_x = Int.of_float (Body.x body) - name_w / 2 in
    let name_y = Int.of_float (Body.b body) + 5 in
    View.draw_text view (name_x, name_y) frame_color name)

let for_env obj = (obj.name, obj.handle, obj.body)