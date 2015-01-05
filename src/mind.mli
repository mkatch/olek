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
    -> (module Mind_core.MIND with type state = 's and type msg = 'm) -> 's link 
end

module type MIND = Mind_core.MIND
module type MIND_INSTANCE = Mind_core.MIND_INSTANCE
type mind = (module MIND_INSTANCE)

val make_mind : (module MIND with type state = 'a) -> 'a -> mind