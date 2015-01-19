open Sdlevent

type t

val empty : t

val text : t -> string

val pos : t -> int

val command : t -> string

val process_keydown_event : keyboard_event -> t -> t