open! Schedulr.Scheduler
open Schedulr.Instance

let log s =
  Printf.printf s;
  Stdlib.flush Stdlib.stdout

let counter = Atomic.make 0;;
let counter_done = Atomic.make 0;;

let () =
  let scheduler_1 = 
    FIFO.init ~afterwards:`return 3 ~f:(fun () -> 
      log "starting 1\n") 
  in 
  let scheduler_2 = 
    FIFO.init ~afterwards:`return 3 ~f:(fun () -> 
      log "starting 2\n") 
  in 
  let incr_and_print () = 
    let old = Atomic.fetch_and_add counter 1 in 
    Printf.printf "%d\n" old;
    Stdlib.flush Stdlib.stdout;
    Atomic.incr counter_done;
  in
  FIFO.inject_task scheduler_1 incr_and_print;
  FIFO.inject_task scheduler_2 incr_and_print;
  FIFO.inject_task scheduler_1 incr_and_print;
  log "scheduled\n";
  while Atomic.get counter_done < 3 do () done;;
  log "exit \n";
  ();;