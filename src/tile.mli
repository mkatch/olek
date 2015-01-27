type t =
  | Void
  | Solid
  | TopSolid
  | Sticky
with sexp

val size : int

val to_int : t -> int
val of_int : int -> t

val is_solid : t -> bool
val is_l_solid : t -> bool
val is_t_solid : t -> bool
val is_r_solid : t -> bool
val is_b_solid : t -> bool