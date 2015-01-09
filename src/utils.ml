open Core.Std
include Utils_sub

type vector = Vector.vector
let make_v = Vector.make
let ( +^ ) = Vector.( +^ )
let ( -^ ) = Vector.( -^ )
let ( *^ ) = Vector.( *^ )

let rec unzip3 = function
  | [] -> ([], [], [])
  | (x, y, z) :: ts ->
    let xs, ys, zs = unzip3 ts in
    (x :: xs, y :: ys, z :: zs)

let triple_of_list_exn = function
  | [x; y; z] -> (x, y, z)
  | _ -> failwith "Utils.triple_of_list_exn: The list does not have 3 elements"

let option_value_exn x = Option.value_exn x