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
