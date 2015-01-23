open Core.Std

module rec Mind_core : sig
  module type MIND = sig
    type state
    type msg with sexp
    type init with sexp

    val default_state : state
    val default_body : Body.t

    val init : state -> Body.t -> init -> state Command.chain
    val think : state -> Body.t -> Env.t -> state Command.chain
    val react : state -> Body.t -> Env.t -> Objevent.t -> state Command.chain
  end
  module type MIND_INSTANCE = sig
    module Mind : MIND
    val state : Mind.state
  end
end = struct
  module type MIND = sig
    type state
    type msg with sexp
    type init with sexp

    val default_state : state
    val default_body : Body.t

    val init : state -> Body.t -> init -> state Command.chain
    val think : state -> Body.t -> Env.t -> state Command.chain
    val react : state -> Body.t -> Env.t -> Objevent.t -> state Command.chain
  end
  module type MIND_INSTANCE = sig
    module Mind : MIND
    val state : Mind.state
  end
end
and Command : sig
  type t =
    | Print of string
    | Focus
  type 's chain
  
  val nop : 's chain

  val get_commands : ?commands:t list -> 's chain -> t list
  val get_state : 's chain -> 's option
  val get_body : 's chain -> Body.t option

  val ( >> ) : 's chain -> 's chain -> 's chain

  val set_state : 's -> 's chain
  val set_body : Body.t -> 's chain
  val set : 's -> Body.t -> 's chain

  val print : string -> 's chain

  val focus : 's chain

  (*
  val send : 'm -> int
    -> (module Mind_core.MIND with type state = 's and type msg = 'm) -> 's link *)
end = struct
  type t =
    | Print of string
    | Focus

  type 's chain =
    | Nop
    | Command of t
    | SetState of 's
    | SetBody of Body.t
    | Node of 's chain * 's chain

  let nop = Nop

  let get_commands ?commands:(commands = []) chain =
    let rec aux chain commands = match chain with
      | Command command -> command :: commands
      | Node (chain1, chain2) -> aux chain1 (aux chain2 commands)
      | _ -> commands
    in aux chain commands 

  let rec get_state = function
    | SetState state -> Some state
    | Node (chain1, chain2) ->
      let state = get_state chain2 in
      if Option.is_some state then state
      else get_state chain1
    | _ -> None

  let rec get_body = function
    | SetBody body -> Some body
    | Node (chain1, chain2) ->
      let body = get_body chain2 in
      if Option.is_some body then body
      else get_body chain1 
    | _ -> None

  let ( >> ) chain1 chain2 =
    if chain1 = Nop then chain2 else
    if chain2 = Nop then chain1
    else Node (chain1, chain2)

  let set_state state = SetState state
  let set_body body = SetBody body
  let set state body = SetState state >> SetBody body

  let print text = Command (Print text)

  let focus = Command Focus

  (*
  let send (type s) (type m) message receiver
    (module M : Mind_core.MIND with type state = s and type msg = m) =
    Inner (Message (receiver, M.msg_to_int message))
  *)
end

include Mind_core
type mind = (module MIND_INSTANCE)

let make (type s) (module M : MIND with type state = s) state = (
  module struct
    module Mind = M
    let state = state
  end : MIND_INSTANCE
)