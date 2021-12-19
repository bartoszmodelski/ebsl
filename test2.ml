open Core
open! Conc_effects_mtx_mult_stl

let start = ref (Core.Time.now ())

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
    dump_stats ();
    Unix.sleep 2;
    monitor ())

let rec f ~n () =
  schedule (fun () -> 
    if n-1 > 0 
    then (
      schedule (fun () -> f ~n:(n-1) ());
      schedule (fun () -> f ~n:(n-1) ()))
    else (
      Sys.opaque_identity ()));;

init 10 ~f:(fun () -> 
  let n = 24 in
  log "starting\n";
  start := Core.Time.now ();
  monitor ();
  f ~n ();
  log "scheduled\n";
  Stdlib.read_line () |> ignore;
  log "exit \n";
  ())