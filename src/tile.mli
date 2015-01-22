type t =
  | Void
  | Solid
  | TopSolid
  | Sticky
with sexp

val size : int

val to_int : t -> int

val of_int : int -> t