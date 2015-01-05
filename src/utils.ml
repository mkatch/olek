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