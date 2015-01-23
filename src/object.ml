open Core.Std
open Utils
open Mind

let make ~pos ~init (module M : MIND) =
  let body = Body.set_pos pos M.default_body in
  let state = M.default_state in
  let init = M.init_of_sexp init in 
  let chain = M.init state body init in
  let state = Option.value (Command.get_state chain) ~default: state in
  let body = Option.value (Command.get_body chain) ~default: body in
  let commands = Command.get_commands chain in
  (body, Mind.make (module M) state, commands)

let think env body mind commands =
  let (module I : MIND_INSTANCE) = mind in
  let chain = I.Mind.think I.state body env in
  let state = Option.value (Command.get_state chain) ~default: I.state in
  let body = Option.value (Command.get_body chain) ~default: body in
  let commands = Command.get_commands chain ~commands:commands in
  (body, Mind.make (module I.Mind) state, commands)

let react env events body mind =
  let (module I : MIND_INSTANCE) = mind in
  let rec aux body state commands = function
    | [] -> (body, Mind.make (module I.Mind) state, commands)
    | event :: events ->
      let chain = I.Mind.react state body env event in
      let state = Option.value (Command.get_state chain) ~default: state in
      let body = Option.value (Command.get_body chain) ~default: body in
      let commands = Command.get_commands chain ~commands:commands in
      aux body state commands events in
  aux body I.state [] events