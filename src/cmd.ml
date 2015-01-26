open Core.Std
open Utils

type t =
  | Message of Sexp.t * Env.handle
  | Spawn of string option * string * (int * int) * Sexp.t
  | Print of string
  | Focus

type 's chain =
  | Nop
  | Command of t
  | SetState of 's
  | SetBody of Body.t
  | Node of 's chain * 's chain

let nop = Nop

(* The commands are returned in reverse order! *)
let get_cmds ?cmds:(cmds = []) chain =
  let rec aux chain cmds = match chain with
    | Command cmd -> cmd :: cmds
    | Node (chain1, chain2) -> aux chain2 (aux chain1 cmds)
    | _ -> cmds
  in aux chain cmds 

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

let send msg receiver = Command (Message (msg, receiver))

let spawn ?name:(name = "")
          ?pos:(pos = (0., 0.))
          ?init:(init = Sexp.unit)
          mind_name =
  let name = if name = "" then None else Some name in
  Command (Spawn (name, mind_name, v_to_ints pos, init))

let print text = Command (Print text)

let focus = Command Focus
