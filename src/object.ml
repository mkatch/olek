open Core.Std
open Utils
open Mind

type t = {
  body : Body.t;
  mind : mind;
}

let body obj = obj.body
let set_body body obj = { obj with body }

let make ~pos ~init (module M : MIND) =
  let body = Body.set_pos pos M.default_body in
  let state = M.default_state in
  let init = M.init_of_sexp init in 
  let chain = M.init state body init in
  let state = Option.value (Command.get_state chain) ~default:state in
  let body = Option.value (Command.get_body chain) ~default:body in
  let mind = Mind.make (module M) state in
  let commands = Command.get_commands chain in
  ({ body; mind } , commands)

let think env obj commands =
  let (module I : MIND_INSTANCE) = obj.mind in
  let chain = I.Mind.think I.state obj.body env in
  let state = Option.value (Command.get_state chain) ~default:I.state in
  let body = Option.value (Command.get_body chain) ~default:obj.body in
  let mind = Mind.make (module I.Mind) state in
  let commands = Command.get_commands chain ~commands:commands in
  ({ body; mind }, commands)

let react env events obj =
  let (module I : MIND_INSTANCE) = obj.mind in
  let rec aux body state commands = function
    | [] ->
      let mind = Mind.make (module I.Mind) state in
      ({ body; mind }, commands)
    | event :: events ->
      let chain = I.Mind.react state body env event in
      let state = Option.value (Command.get_state chain) ~default:state in
      let body = Option.value (Command.get_body chain) ~default:obj.body in
      let commands = Command.get_commands chain ~commands:commands in
      aux body state commands events in
  aux obj.body I.state [] events

let advance_sprite t obj = { obj with body = Body.advance_sprite t obj.body }

let draw view obj = Body.draw obj.body view ~draw_bbox:true