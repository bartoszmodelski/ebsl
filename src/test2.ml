open Core
open! Conc_effects_lf_mult_stl

let start = ref (Core.Time.now ())

let counter = Atomic.make 0;;

let log s =
  Printf.printf s;
  Stdlib.flush Stdlib.stdout;;

let rec monitor () =
  schedule (fun () ->
    Printf.printf "\n\n-----------------------\n";    
    let diff = Core.Time.diff (Core.Time.now ()) !start in
    Printf.printf "  time diff: %s\n" (Core.Time.Span.to_string_hum diff);
    let ({live_words; _} : Gc.stat)= Gc.stat () in 
    Printf.printf "  live_words: %d\n" live_words;
    Printf.printf "  counter: %d\n" (Atomic.get counter);
    Stdlib.flush Stdlib.stdout;
    Unix.sleep 2;
    monitor ())

let rec f ~n () =
  schedule (fun () -> 
    if n-1 > 0 
    then (
      schedule (fun () -> f ~n:(n-1) ());
      schedule (fun () -> f ~n:(n-1) ()))
    else (
      Atomic.incr counter;
      Sys.opaque_identity ()));;

let () = init 10 ~f:(fun () -> 
  let n = 24 in
  log "starting\n";
  start := Core.Time.now ();
  monitor ();
  f ~n ();
  log "scheduled\n";
  let _a = Stdlib.read_line () in
  log "exit \n";
  ());;