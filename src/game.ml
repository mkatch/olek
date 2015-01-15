let main () =
  Random.self_init ();
  let rec loop f t state =
    match Engine.iter state with
    | None -> Engine.quit ()
    | Some state ->
      let c = Unix.time () in
      if t +. 1. <= c then begin
        Printf.printf "fps: %d; %f\n%!" f c;
        loop 1 c state
      end else
        loop (f + 1) t state
  in loop 1 (Unix.time ()) (Engine.init ~w:800 ~h:600 ~fps:30)

let () = main ()