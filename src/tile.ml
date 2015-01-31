open Core.Std

type t =
  | Void
  | Solid
  | TopSolid
  | Deadly

let sexp_of_t = function
  | Void -> Sexp.Atom "."
  | Solid -> Sexp.Atom "#"
  | TopSolid -> Sexp.Atom "^"
  | Deadly -> Sexp.Atom "X"

let t_of_sexp = function
  | Sexp.Atom s -> (
    match String.nget s 0 with
    | '.' -> Void
    | '#' -> Solid
    | '^' -> TopSolid
    | 'X' -> Deadly
    | _ -> failwith "Tile.t_of_exp: Invalid tile symbol"
  )
  | _ -> failwith "Tile.t_of_exp: Invalid expression"

let size = 16

let to_int = function
  | Solid -> 0
  | TopSolid -> 1
  | Deadly -> 2
  | Void -> 3

let of_int = function
  | 0 -> Solid
  | 1 -> TopSolid
  | 2 -> Deadly
  | _ -> Void

let is_solid = function
  | Solid
  | Deadly -> true
  | _ -> false

let is_l_solid = function
  | Solid
  | Deadly -> true
  | _ -> false

let is_t_solid = function
  | Solid
  | TopSolid
  | Deadly -> true
  | _ -> false

let is_r_solid = function
  | Solid
  | Deadly -> true
  | _ -> false

let is_b_solid = function
  | Solid
  | Deadly -> true
  | _ -> false