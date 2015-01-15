type state

val init : w:int -> h:int -> fps:int -> state

val quit : unit -> unit

val iter : state -> state option