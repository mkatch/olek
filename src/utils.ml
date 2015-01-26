open Core.Std
include Utils_sub

module StringMap = Map.Make(String)

include Vector

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

let actual_mod a b =
  let b = abs b in
  if a >= 0 then a mod b
  else (b - ((-a) mod b)) mod b 

module Sdlvideo =
struct
  include Sdlvideo

  let nil_rect = Sdlvideo.rect (-1) (-1) (-1) (-1)

  let gray = (128, 128, 128)

  type int_triple = int * int * int with sexp

  let color_of_sexp = int_triple_of_sexp

  let sexp_of_color = sexp_of_int_triple

  let inflate_rect amt { r_x; r_y; r_w; r_h; } =
    rect ~x:(r_x - amt) ~y:(r_y - amt) ~w:(r_w + 2 * amt) ~h:(r_h + 2 * amt)

  let rect_corners { r_x; r_y; r_w; r_h; } =
    (r_x, r_y), (r_x + r_w, r_y + r_h)

  let move_rect (dx, dy) rect =
    { rect with r_x = rect.r_x + dx; r_y = rect.r_y + dy }
end