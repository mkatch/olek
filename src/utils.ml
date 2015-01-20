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

let between ~min ~max x =
  min <= x && x <= max

let clamp ~min ~max x =
  if x <= min then min else
  if x >= max then max
  else x

let filename_concat = function
  | [] -> ""
  | [f] -> f
  | f :: fs -> List.fold fs ~init:f ~f:Filename.concat

let list_rem i xs = List.filteri ~f:(fun j _ -> j <> i) xs

let list_insert x i xs =
  let h, t = List.split_n xs i in
  h @ (x :: t)

module Sdlvideo =
struct
  include Sdlvideo

  type int_triple = int * int * int with sexp

  let color_of_sexp = int_triple_of_sexp

  let sexp_of_color = sexp_of_int_triple
end