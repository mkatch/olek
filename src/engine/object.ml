module type Mind = sig
  type t
  type obj = { body : Body.t; mind : t }
  val init : Body.t -> obj
  val think : obj -> obj
end

module type Mind_instance = sig
  module Mind : Mind
  val this : Mind.t
end

type t = {
  body : Body.t;
  mind : (module Mind_instance);
}

let make_mind_instance (type a) (module M : Mind with type t = a) this = (
  module struct
    module Mind = M
    let this = this
  end : Mind_instance
)

let create pos (module M : Mind) =
  let obj = M.init (Body.make pos 0 0)
  in {
    body = obj.M.body;
    mind = make_mind_instance (module M) obj.M.mind
  }

let body obj = obj.body

let think obj =
  let (module I : Mind_instance) = obj.mind in
  let obj' = I.Mind.think {I.Mind.body = obj.body; I.Mind.mind = I.this}
  in {
    body = obj'.I.Mind.body;
    mind = make_mind_instance (module I.Mind) obj'.I.Mind.mind
  }