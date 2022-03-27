
let total_wait = Atomic.make 0

let _done = Atomic.make 0;;
let process ~start_time () = 
  Unix.sleepf 0.01;
  Micropools.schedule (fun () -> 
    Unix.sleepf 0.01;
    let end_time = Schedulr.Fast_clock.now () in 
    let _diff =  Base.Int63.(to_int64(end_time-start_time)) in 
    Atomic.incr _done);;

let accept ~start_time () = 
  Unix.sleepf 0.001;
  Micropools.schedule (fun () -> 
    Micropools.schedule ~pool_name:"process" (process ~start_time));;

let total_calls = 100
let bench () = 
  (* touch pools first *)
  Micropools.schedule ~pool_name:"process" (fun () -> ());
  Micropools.schedule ~pool_name:"accept" (fun () -> ());
  (* bench *)
  for _ = 1 to total_calls do
    let start_time = Schedulr.Fast_clock.now () in  
    Micropools.schedule ~pool_name:"accept" (accept ~start_time);
  done;
  while Atomic.get _done < total_calls do () done;
  Printf.printf "done\n";
  Stdlib.flush_all ();;

let () =
  bench () 