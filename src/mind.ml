module rec Mind_core : sig
  module type MIND = sig
    type state
    type msg
    val msg_from_int : int -> msg
    val msg_to_int : msg -> int
    val init : Body.t -> state Command.chain
    val think : Body.t -> state -> Env.t -> state Command.chain
    val react : Body.t -> state -> Objevent.t -> Env.t -> state Command.chain
  end
  module type MIND_INSTANCE = sig
    module Mind : MIND
    val state : Mind.state
  end
end = struct
  module type MIND = sig
    type state
    type msg
    val msg_from_int : int -> msg
    val msg_to_int : msg -> int
    val init : Body.t -> state Command.chain
    val think : Body.t -> state -> Env.t -> state Command.chain
    val react : Body.t -> state -> Objevent.t -> Env.t -> state Command.chain
  end
  module type MIND_INSTANCE = sig
    module Mind : MIND
    val state : Mind.state
  end
end
and Command : sig
  type t =
    | Nop
    | Message of int * int
  type 's link
  type 's chain
  val start : 's chain
  val return : Body.t -> 's -> 's link
  val just_return : Body.t -> 's -> 's chain
  val ( |+ ) : 's chain -> 's link -> 's chain
  val chain_snoc : 's chain -> Body.t * 's * t list
  val nop : 's link
  val send : 'm -> int
    -> (module Mind_core.MIND with type state = 's and type msg = 'm)
    -> 's link
end = struct
  type t =
    | Nop
    | Message of int * int

  type 's link =
    | Inner of t
    | Closing of Body.t * 's

  type 's chain =
    | Open of t list
    | Closed of Body.t * 's * t list

  let start = Open []

  let return body state = Closing (body, state)

  let just_return body state = Closed (body, state, [])

  let ( |+ ) chain link = match chain, link with
    | Open cmds, Inner cmd -> Open (cmd :: cmds)
    | Open cmds, Closing (body, state) -> Closed (body, state, cmds)
    | Closed _, _ -> failwith "Command.( |+ ): The chain is closed"

  let chain_snoc = function
    | Closed (body, state, cmds) -> (body, state, cmds)
    | _ -> failwith "Command.chain_snoc: The chain is not closed"

  let nop = Inner Nop

  let send (type s) (type m) message receiver
    (module M : Mind_core.MIND with type state = s and type msg = m) =
    Inner (Message (receiver, M.msg_to_int message))
end

include Mind_core
type mind = (module MIND_INSTANCE)

let make_mind (type s) (module M : MIND with type state = s) state = (
  module struct
    module Mind = M
    let state = state
  end : MIND_INSTANCE
)