open Core.Std

exception Empty

type 'a t = {
  array : 'a Array.t;
  cols : int
}

let of_lists xss =
  let cols = match List.hd xss with
    | None -> 0
    | Some xs -> List.length xs in
  if List.for_all ~f:(fun xs -> List.length xs = cols) xss then
    let array = Array.of_list (List.concat xss)
    in {array; cols}
  else invalid_arg "grid_of_ists: Inconsistent row lengths"

let iteri ~f ~r_beg ~c_beg ~r_end ~c_end {array; cols} =
  for i = r_beg to r_end do
    for j = c_beg to c_end do
      f i j (Array.get array (i * cols + j))
    done
  done
  
(*
type 'a tree =
| Nil
| Leaf of 'a
| Node of 'a tree * 'a tree * 'a tree * 'a tree

type 'a t = 'a tree

let hsplit xs =
  let rec aux xs ys zs = match xs, ys with
  | [], _ -> (List.rev zs, ys)
  | _ :: ([] as xs'), y :: ys'
  | _ :: _ :: xs', y :: ys' -> aux xs' ys' (y :: zs)
  | _, [] -> failwith "hsplit: Internal error"
  in aux xs xs []

let qsplit xss =
  let xss21, xss34 = hsplit xss in
  let xss2, xss1 = List.unzip (List.map ~f:hsplit xss21) in
  let xss3, xss4 = List.unzip (List.map ~f:hsplit xss34)
  in (xss1, xss2, xss3, xss4)

let rec tree_of_lists = function
| [] | [[]] -> Nil
| [[x]] -> Leaf x
| xss ->
  let xss1, xss2, xss3, xss4 = qsplit xss
  in Node (
    tree_of_lists xss1,
    tree_of_lists xss2,
    tree_of_lists xss3,
    tree_of_lists xss4
  )

type 'a grid = {
  tree : 'a tree;
  rows : int;
  cols : int;
}
type 'a zipper = {
  value: 'a;
  up: 'a uptree;
}
and 'a uptree =
| UpRoot of 'a tree
| UpNode of 'a uptree * 'a tree * int

let grid_of_lists xss =
  let rows = List.length xss in
  let cols = match List.hd xss with
  | None -> 0
  | Some xs -> List.length xs in
  if List.for_all ~f:(fun xs -> List.length xs = cols) xss then
    {tree = tree_of_lists xss; rows; cols}
  else invalid_arg "grid_of_ists: Inconsistent row lengths"

let focus grid x y =
  let rec aux up = function
  | Nil -> raise Empty
  | Leaf value -> {value; up}
  | Node (_, tree, _, _) -> aux (UpNode (up, tree, 2)) tree
  in aux (UpRoot grid.tree) grid.tree

let gu { cell = _; up = u } =
  let rec sink u q moves = match q, moves with
    | QLeaf c, [] -> { cell = c; up = u }
    | QNode (_, _, q3, q4), m :: moves' when m = 1 || m = 2 ->
      if m = 1 then sink (QUpNode (u, q4, 4)) q4 moves'
      else sink (QUpNode (u, q3, 3)) q4 moves'
    | _ -> failwith "gu: Internal error" in
  let rec emerge u moves = match u with
    | QUpRoot _ -> failwith "gu: No neighbor above"
    | QUpNode (u', QNode (q1, q2, _, _), m) ->
      if m = 3 then sink (QUpNode (u, q2, 2)) q2 moves else
      if m = 4 then sink (QUpNode (u, q1, 1)) q1 moves
      else emerge u' (m :: moves)
    | _ -> failwith "gu: Internal error1"
  in emerge u []



let g = grid_of_lists xss
let gz = make_gzipper g

let xss = [
  [ 1;  2;  3;  4;  5];
  [ 6;  7;  8;  9; 10];
  [11; 12; 13; 14; 15];
  [16; 17; 18; 19; 20]
]
*)