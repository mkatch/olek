open Utils

type t = {
  c: Vector.t;
  r: float;
  a: float;
  da: float
}

type obj = { body : Body.t; mind : t }

let init body = {
  body = Body.{ body with w = Random.int 3 + 1; h = Random.int 3 + 1 };
  mind = {
    c = body.Body.pos;
    r = Random.float 20. +. 30.;
    a = 0.;
    da = Random.float 0.1;
  }
}

let think {body; mind} =
  let s = sin mind.a in
  let c = cos mind.a in
  let offset = Vector.(mind.r * make s c)
  in {
    body = Body.move_to body Vector.(mind.c + offset);
    mind = {mind with a = mind.a +. mind.da};
  }