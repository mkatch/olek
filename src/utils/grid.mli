type 'a t

val of_lists : 'a list list -> 'a t

val iteri :
  f:(int -> int -> 'a -> unit) ->
  r_beg:int -> c_beg:int ->
  r_end:int -> c_end:int ->
  'a t -> unit