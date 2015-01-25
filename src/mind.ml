open Core.Std
open Utils

module type MIND = sig
  type state
  type init with sexp
  type msg with sexp

  val name : string
  val default_state : state
  val default_body : Body.t
  val default_init : init

  val init : state -> Body.t -> init -> state Cmd.chain
  val think : state -> Body.t -> Env.t -> state Cmd.chain
  val react : state -> Body.t -> Env.t -> Objevent.t -> state Cmd.chain
  val receive : state -> Body.t -> Env.t -> Env.handle -> msg -> state Cmd.chain
end

module type INSTANCE = sig
  module Mind : MIND
  val state : Mind.state
end

let make_instance (type s) (module M : MIND with type state = s) state = (
  module struct
    module Mind = M
    let state = state
  end : INSTANCE
)

type mind = (module MIND)
type instance = (module INSTANCE)

include Minds

let mind_list = [
  (module Dummy : MIND);
]
let minds =
  let make_kv m = let (module M : MIND) = m in (M.name, m) in
  StringMap.of_alist_exn (List.map ~f:make_kv mind_list)

let find name = StringMap.find minds name
let find_exn name = StringMap.find_exn minds name

let mind_of_sexp sexp = find_exn (string_of_sexp sexp)

let sexp_of_mind mind =
  let (module M : MIND) = mind in
  sexp_of_string M.name