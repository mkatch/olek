open Core.Std
open Utils
open Mind
include Minds_sub

let dummy = (module Dummy : MIND)

let mind_list = [
  dummy;
]

let minds =
  let make_kv m = let (module M : MIND) = m in (M.name, m) in
  StringMap.of_alist_exn (List.map ~f:make_kv mind_list)