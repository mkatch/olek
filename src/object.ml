open Utils
open Mind

let create pos (module M : MIND) =
  let body = Body.make pos 0 0 in
  let cmd_chain = M.init body in
  let body, state, cmds = Command.chain_snoc cmd_chain in
  (body, Mind.make_mind (module M) state, cmds)

let dispatch_events events env body mind =
  let (module I : MIND_INSTANCE) = mind in
  let rec aux body state rcmds = function
    | [] -> (body, Mind.make_mind (module I.Mind) state, List.rev rcmds)
    | event :: events ->
      let cmd_chain =
        if Objevent.is_next_frame event then
          I.Mind.think body state env
        else
          I.Mind.react body state event env in
      let body, state, cmds = Command.chain_snoc cmd_chain in
      aux body state (List.rev_append cmds rcmds) events in
  aux body I.state [] events