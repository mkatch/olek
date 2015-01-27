type 'a t with sexp

val row_cnt : 'a t -> int
val column_cnt : 'a t -> int
val dims : 'a t -> int * int

val make : 'a -> int -> int -> 'a t

val are_coords_valid : int -> int -> 'a t -> bool

val of_lists : 'a list list -> 'a t

val get : int -> int -> 'a t -> 'a
val get_safe : int -> int -> default:'a -> 'a t -> 'a

val set : int -> int -> 'a -> 'a t -> 'a t

val iteri :
  f:(int -> int -> 'a -> unit) ->
  ?r_beg:int -> ?c_beg:int ->
  ?r_end:int -> ?c_end:int ->
  'a t -> unit

val map : f:('a -> 'b) -> 'a t -> 'b t