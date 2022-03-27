
let total_wait = Atomic.make 0
let _done = Atomic.make 0;;

let process_pool = ref None 
let decode_pool = ref None 
let accept_pool = ref None 
let time_pool = ref None 
let hist = Schedulr.Histogram.init ~size:48 ()

let process ~start_time () = 
  Micropools.schedule ?pool_name:!time_pool (fun () -> 
    let end_time = Schedulr.Fast_clock.now () in 
    let diff =  Base.Int63.(to_int_exn(end_time-start_time)) in 
    Schedulr.Histogram.log_val hist (diff + 1);
    Atomic.incr _done);;

let accept ~start_time () = 
  Micropools.schedule ?pool_name:!decode_pool (fun () -> 
    Micropools.schedule ?pool_name:!process_pool (process ~start_time));;

let total_calls = 1000000

let pool_size = ref 1
let ready = Atomic.make 0 

let bench () =
  Micropools.schedule ~pool_size:!pool_size ~pool_name:"general" (fun () -> 
    if Option.is_some !process_pool 
    then 
      ((* touch pools first *)
      let start_f = fun () -> Atomic.incr ready in 
      Micropools.schedule ~pool_size:!pool_size ?pool_name:!process_pool start_f;
      Micropools.schedule ~pool_size:!pool_size ?pool_name:!decode_pool start_f;
      Micropools.schedule ~pool_size:!pool_size ?pool_name:!accept_pool start_f;
      Micropools.schedule ~pool_size:!pool_size ?pool_name:!time_pool start_f;
      (* wait for setup *)
      while Atomic.get ready < 4 do () done);
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
    time_pool := Some "time";
    pool_size := 1) 
  | `Single -> 
    (process_pool := None;
    decode_pool := None;
    accept_pool := None;
    pool_size := 4);;

let () =
  set_mode `Single;
  bench (); 
  Schedulr.Histogram.dump hist;;