type 'a oref = 'a option ref

exception OrefEmpty

let oref x = ref (Some x)

let empty_oref () = ref None

let ( !? ) x = match !x with
  | None -> raise OrefEmpty
  | Some value -> value

let ( <?- ) x y = x := Some y