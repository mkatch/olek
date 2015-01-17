type sheet

type instance

val dummy_sheet : sheet

val dummy_instance : instance

val make_sheet : image:string -> frames:int -> dt:int -> sheet

val make_instance : sheet -> instance

val sheet_dims : sheet -> int * int

val dims : instance -> int * int

val advance : int -> instance -> instance

val blit_data : instance -> Sdlvideo.surface * Sdlvideo.rect