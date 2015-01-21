open Utils

type t

val make : vector -> t

val offset : t -> vector

val int_offset : t -> int * int

val to_world : t -> vector -> vector

val to_view : t -> vector -> vector

val focus : vector -> float -> t -> t