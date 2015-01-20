open Core.Std

exception Empty

type 'a t = {
  array : 'a Array.t;
  rows : int;
  cols : int;
}

let make x rows cols =
  let array = Array.init (rows * cols) ~f:(fun _ -> x) in
  { array; rows; cols; }

let ind i j g = i * g.cols + j

let get i j g = Array.get g.array (ind i j g)

let set i j x g = Array.set g.array (ind i j g) x; g 

let of_lists xss =
  let cols = match List.hd xss with
    | None -> 0
    | Some xs -> List.length xs in
  let rows = List.length xss in
  if List.for_all ~f:(fun xs -> List.length xs = cols) xss then
    let array = Array.of_list (List.concat xss)
    in { array; rows; cols }
  else invalid_arg "Grid.of_lists: Inconsistent row lengths"

let iteri ~f
          ?r_beg:(r_beg =  0) ?c_beg:(c_beg =  0)
          ?r_end:(r_end = -1) ?c_end:(c_end = -1)
          { array; rows; cols } =
  let r_end = if r_end <= r_beg then (rows - 1) else r_end - 1 in
  let c_end = if c_end <= c_beg then (cols - 1) else c_end - 1 in
  for i = r_beg to r_end do
    for j = c_beg to c_end do
      f i j (Array.get array (i * cols + j))
    done
  done

let map ~f g = { g with array = Array.map ~f:f g.array }

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