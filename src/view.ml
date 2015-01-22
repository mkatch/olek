open Utils

type t = {
  offset : vector;
}

let make offset = { offset }

let offset view = view.offset

let int_offset view = Vector.to_ints view.offset

let to_world view pos = pos +^ view.offset

let to_view view pos = pos -^ view.offset

let focus offset t view = { offset = 0.05 *^ offset +^ 0.95 *^ view.offset}