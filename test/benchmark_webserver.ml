
let total_wait = Atomic.make 0
let _done = Atomic.make 0;;

let process_pool = ref None 
let decode_pool = ref None 
let accept_pool = ref None 
let time_pool = ref None 

let process ~start_time () = 
  Micropools.schedule ?pool_name:!time_pool (fun () -> 
    let end_time = Schedulr.Fast_clock.now () in 
    let diff =  Base.Int63.(to_int_exn(end_time-start_time)) in 
    let hist = Schedulr.Histogram.Per_thread.get_hist () in 
    Schedulr.Histogram.log_val hist (diff + 1);
    Atomic.incr _done);;

let accept ~start_time () = 
  Micropools.schedule ?pool_name:!accept_pool (fun () ->
    Micropools.schedule ?pool_name:!decode_pool (fun () -> 
      Micropools.schedule ?pool_name:!process_pool (process ~start_time)));;


let total_calls = 10000000

let pool_size = ref 1
let ready = Atomic.make 0 

let bench () =
  Micropools.schedule ~pool_size:!pool_size ~pool_name:"general" (fun () -> 
    if Option.is_some !process_pool 
    then 
      ((* touch pools first *)
      let start_f = fun () -> Atomic.incr ready in 
      Micropools.schedule ~pool_size:1 ?pool_name:!process_pool start_f;
      Micropools.schedule ~pool_size:1 ?pool_name:!decode_pool start_f;
      Micropools.schedule ~pool_size:1 ?pool_name:!accept_pool start_f;
      Micropools.schedule ~pool_size:1 ?pool_name:!time_pool start_f;
      (* wait for setup *)
      while Atomic.get ready < 4 do () done);
    (* bench *)

    for _ = 1 to 10 do 
      let start_time = Schedulr.Fast_clock.now () in  
      for _ = 1 to total_calls/10 do
        accept ~start_time ();
      done;
      Unix.sleepf 0.00001; 
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
  Schedulr.Histogram.Per_thread.init 10;
  let usage_msg = "benchmark -mode (pools|single)" in 
  let mode = ref "" in
  let speclist =
    [("-mode", Arg.Set_string mode, "set mode")] 
  in 
  Arg.parse speclist Flags.anon_fun usage_msg;
  let mode =
      match !mode with 
      | "pools" -> `Pools
      | "single" -> `Single
      | _ -> assert false
  in 
  set_mode mode;
  bench (); 
  (*Schedulr.Histogram.Per_thread.dump_each ();*)
  let merged = Schedulr.Histogram.Per_thread.all () in 
  Schedulr.Histogram.dump merged;
  let f v = 
    Schedulr.Histogram.quantile ~quantile:v merged
  in
  Printf.printf ".5: %d, .99: %d, .999: %d, 9995: %d\n" (f 0.5) (f 0.99) (f 0.999) (f 0.9995);;