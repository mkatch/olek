open Sexplib
open Sexp

type 'a t

val make : 'a -> int -> int -> 'a t

(*
val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t

val t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a t
*)
val of_lists : 'a list list -> 'a t

val get : int -> int -> 'a t -> 'a

val set : int -> int -> 'a -> 'a t -> 'a t

val iteri :
  f:(int -> int -> 'a -> unit) ->
  ?r_beg:int -> ?c_beg:int ->
  ?r_end:int -> ?c_end:int ->
  'a t -> unit

val map : f:('a -> 'b) -> 'a t -> 'b t