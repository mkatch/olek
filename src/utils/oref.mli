type 'a oref

exception OrefEmpty

val oref : 'a -> 'a oref

val empty_oref : unit -> 'a oref

val ( !? ) : 'a oref -> 'a

val ( <?- ) : 'a oref -> 'a -> unit