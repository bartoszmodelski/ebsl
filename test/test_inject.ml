open! Schedulr.Scheduler
open Schedulr.Instance

let log s =
  Printf.printf s;
  Stdlib.flush Stdlib.stdout

let counter = Atomic.make 0;;

let () =
  let scheduler = 
    FIFO.init ~afterwards:`return 3 ~f:(fun () -> 
      log "starting\n") 
  in 
  let incr_and_print () = 
    let old = Atomic.fetch_and_add counter 1 in 
    Printf.printf "%d\n" old;
    Stdlib.flush Stdlib.stdout;
  in
  FIFO.inject_task scheduler incr_and_print;
  FIFO.inject_task scheduler incr_and_print;
  FIFO.inject_task scheduler incr_and_print;
  log "scheduled\n";
  while Atomic.get counter < 3 do () done;;
  Unix.sleep 1;
  log "exit \n";
  ();;