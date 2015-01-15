type 'a t

val of_lists : 'a list list -> 'a t

val get : int -> int -> 'a t -> 'a

val iteri :
  f:(int -> int -> 'a -> unit) ->
  ?r_beg:int -> ?c_beg:int ->
  ?r_end:int -> ?c_end:int ->
  'a t -> unit

val map : f:('a -> 'b) -> 'a t -> 'b t