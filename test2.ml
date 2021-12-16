open! Concurrent_scheduler_with_effects

let log s =
  Printf.printf s;
  Stdlib.flush Stdlib.stdout;;

let rec monitor () =
  schedule (fun () ->
    Printf.printf "\n\n-----------------------\n";
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
  let n = 23 in
  log "starting\n";
  monitor ();
  f ~n ();
  log "scheduled\n";
  Stdlib.read_line () |> ignore;
  log "exit \n";
  ())