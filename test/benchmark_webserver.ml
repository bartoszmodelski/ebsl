
let total_wait = Atomic.make 0
let _done = Atomic.make 0;;

let process_pool = ref None 
let decode_pool = ref None 
let accept_pool = ref None 

let process ~start_time () = 
  Unix.sleepf 0.0001;
  Micropools.schedule (fun () -> 
    Unix.sleepf 0.0001;
    let end_time = Schedulr.Fast_clock.now () in 
    let _diff =  Base.Int63.(to_int_exn(end_time-start_time)) in
    Printf.printf "%d-" (_diff / 1_000_000); 
    Atomic.incr _done);;

let accept ~start_time () = 
  Unix.sleepf 0.00001;
  Micropools.schedule ?pool_name:!decode_pool (fun () -> 
    Micropools.schedule ?pool_name:!process_pool (process ~start_time));;

let total_calls = 10000

let pool_size = ref 1
let bench () =
  Micropools.schedule ~pool_size:!pool_size ~pool_name:"general" (fun () -> 
    (* touch pools first *)
    Micropools.schedule ~pool_size:!pool_size ?pool_name:!process_pool (fun () -> ());
    Micropools.schedule ~pool_size:!pool_size ?pool_name:!decode_pool (fun () -> ());
    Micropools.schedule ~pool_size:!pool_size ?pool_name:!accept_pool (fun () -> ());
    (* bench *)
    for _ = 1 to total_calls do
      let start_time = Schedulr.Fast_clock.now () in  
      Micropools.schedule ?pool_name:!accept_pool (accept ~start_time);
    done);
  while Atomic.get _done < total_calls do () done;
  Printf.printf "done\n";
  Stdlib.flush_all ();;

let set_mode = function
  | `Pools -> 
    (process_pool := Some "process";
    decode_pool := Some "decode";
    accept_pool := Some "accept";
    pool_size := 1) 
  | `Single -> 
    (process_pool := None;
    decode_pool := None;
    accept_pool := None;
    pool_size := 3);;

let () =
  set_mode `Single;
  bench () 