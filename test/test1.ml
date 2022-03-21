open! Schedulr.Scheduler
open Schedulr.Instance

let log s =
  Printf.printf s;
  Stdlib.flush Stdlib.stdout

let () =
  FIFO.init 3 ~f:(fun () -> 
    log "starting\n";
    schedule (fun () -> 
      log "  start 2\n"; 
      yield (); 
      log "  wake up 2\n"; 
      Unix.sleep 2; 
      yield (); 
      log "  done 2\n") |> ignore;
    schedule (fun () -> 
      log "  start 1\n"; 
      yield (); 
      log "  wake up 1\n"; 
      Unix.sleep 1; 
      yield (); 
      log "  done 1\n") |> ignore;
    schedule (fun () -> 
      log "  start 3\n"; 
      yield (); 
      log "  wake up 3\n";
      Unix.sleep 3; 
      yield ();
      log "  done 3\n") |> ignore;
    log "scheduled\n";
    Unix.sleep 15;
    log "exit \n";
    ())