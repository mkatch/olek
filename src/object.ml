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
  pos : float * float;
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
  let body = Body.set_pos stub.pos M.default_body in
  let state = M.default_state in
  let init = M.init_of_sexp stub.init in 
  let chain = M.init state body init in
  let state = Option.value (Cmd.get_state chain) ~default:state in
  let handle = Env.new_handle () in
  let name = stub.obj_name in
  let body = Option.value (Cmd.get_body chain) ~default:body in
  let mind_instance = Mind.make_instance (module M) state in
  let cmds = Cmd.get_cmds chain in
  ({ handle; name; body; mind_instance; }, cmds)

let think env obj cmds =
  let (module I : Mind.INSTANCE) = obj.mind_instance in
  let chain = I.Mind.think I.state obj.body env in
  let state = Option.value (Cmd.get_state chain) ~default:I.state in
  let body = Option.value (Cmd.get_body chain) ~default:obj.body in
  let mind_instance = Mind.make_instance (module I.Mind) state in
  let cmds = Cmd.get_cmds chain ~cmds:cmds in
  ({ obj with body; mind_instance }, cmds)

let react env events obj cmds =
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

let receive env sender data obj =
  let (module I : Mind.INSTANCE) = obj.mind_instance in
  let msg = I.Mind.msg_of_sexp data in
  let chain = I.Mind.receive I.state obj.body env sender msg in
  let state = Option.value (Cmd.get_state chain) ~default:I.state in
  let body = Option.value (Cmd.get_body chain) ~default:obj.body in
  let mind_instance = Mind.make_instance (module I.Mind) state in
  let cmds = Cmd.get_cmds chain in
  ({ obj with body; mind_instance }, cmds)


let advance_sprite t obj = { obj with body = Body.advance_sprite t obj.body }

let draw view obj = Body.draw view obj.body

let set_stub_name name stub = { stub with obj_name = name }

let draw_stub view stub =
  let (module M : Mind.MIND) = stub.mind in
  let body = Body.set_pos stub.pos M.default_body in
  Body.draw view body;
  View.draw_rect view Sdlvideo.red (Body.rect body);
  match stub.obj_name with
  | None -> ()
  | Some name ->
    let name_w, _ = Canvas.size_text name in
    let name_x = Int.of_float (Body.x body) - name_w / 2 in
    let name_y = Int.of_float (Body.b body) + 5 in
    View.draw_text view (name_x, name_y) Sdlvideo.red name

let for_env obj = (obj.name, obj.handle, obj.body)