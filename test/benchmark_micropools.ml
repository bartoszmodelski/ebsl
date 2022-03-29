
let total_wait = Atomic.make 0
let _done = Atomic.make 0;;

module Pools = struct 
  type t = 
    | Single 
    | Pools
  let mode = ref None

  let set_mode = function
    | `Single -> 
      mode := Some Single
    | `Pools -> 
      mode := Some Pools
  ;;


  let process_pool = "process"
  let decode_pool = "decode"
  let accept_pool = "accept"
  let time_pool = "time"
  let general_pool = "general"

  let sched ?pool_size pool f =
    let pool_name =
      match !mode, pool with 
      | None, _ -> assert false 
      | Some Single, (`Process | `Decode | `Accept | `Time) -> None
      | Some (Single | Pools), `General -> Some general_pool
      | Some Pools, `Process -> Some process_pool
      | Some Pools, `Decode -> Some decode_pool
      | Some Pools, `Accept -> Some accept_pool
      | Some Pools, `Time -> Some time_pool
    in
    Micropools.schedule ?pool_size ?pool_name f

    let sched_accept ?pool_size f = sched `Accept ?pool_size f 
    let sched_process ?pool_size f = sched `Process ?pool_size f 
    let sched_time ?pool_size f = sched `Time ?pool_size f 
    let sched_decode ?pool_size f = sched `Decode ?pool_size f 
    let sched_general ?pool_size f = sched ?pool_size `General f



  let init () = 
    let ready = Atomic.make 0 in
    let start_f = fun () -> Atomic.incr ready in 
    match !mode with 
    | None -> assert false 
    | Some Single -> 
      (sched_general ~pool_size:50 start_f;
      while Atomic.get ready < 1 do () done)
    | Some Pools ->
      ((* touch pools first *)
      let pool_size = 10 in 
      sched_general ~pool_size start_f;
      sched_accept ~pool_size start_f;
      sched_process ~pool_size start_f;
      sched_time ~pool_size start_f;
      sched_decode ~pool_size start_f;
      (* wait for setup *)
      while Atomic.get ready < 4 do () done);;
end


let accept ~start_time () = 
  Pools.sched_accept (fun () ->
    Unix.sleepf 0.000_001;
    Pools.sched_decode (fun () -> 
      Unix.sleepf 0.000_001;
        Pools.sched_process (fun () ->
          Unix.sleepf 0.000_001;
          Pools.sched_time (fun () -> 
            let end_time = Schedulr.Fast_clock.now () in 
            let diff =  Base.Int63.(to_int_exn(end_time-start_time)) in 
            let hist = Schedulr.Histogram.Per_thread.get_hist () in 
            Schedulr.Histogram.log_val hist (diff + 1);
            Atomic.incr _done))));;


let total_calls = 10000000

let pool_size = ref 1

let bench () =
  Pools.init ();
  Unix.sleep 2;
  Pools.sched_general (fun () -> 
    (* bench *)
    for _ = 1 to 10 do 
      let start_time = Schedulr.Fast_clock.now () in  
      for _ = 1 to total_calls/10 do
        accept ~start_time ();
      done;
      (*Unix.sleepf 0.00001;*)
    done);
  while Atomic.get _done < total_calls do () done;
  Printf.printf "done\n";
  Stdlib.flush_all ();;


let () =
  Schedulr.Histogram.Per_thread.init 55;
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
  Pools.set_mode mode;
  bench (); 
  (*Schedulr.Histogram.Per_thread.dump_each ();*)
  let merged = Schedulr.Histogram.Per_thread.all () in 
  Schedulr.Histogram.dump merged;
  let f v = 
    Schedulr.Histogram.quantile ~quantile:v merged
  in
  Printf.printf ".5: %d, .99: %d, .999: %d, 9995: %d\n" 
    (f 0.5) (f 0.99) (f 0.999) (f 0.9995);;