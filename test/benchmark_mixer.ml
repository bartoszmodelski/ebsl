let buffer_size = 1600


let _log s = 
  Printf.printf "%s\n" s;
  Stdlib.(flush stdout);;


let _ = Printexc.record_backtrace true

let empty_buffer () = Bytes.create buffer_size
let random_buffer () = 
  let v = empty_buffer () in 
  for i = 0 to buffer_size / 4 - 5  do 
    try Bytes.set_int64_be v (i * 4) (Random.int64 Int64.max_int) with 
    | Invalid_argument _ -> Stdlib.exit 1;
  done;
  v;;


let realtime_clock = 
  match Core.Unix.Clock.gettime with 
  | Error _ -> assert false 
  | Ok v -> v

let clock () = 
  realtime_clock Core.Unix.Clock.Monotonic
  |> Core.Int63.to_int_exn
;;

let start_leg ~n slot () = 
  let data = ref (empty_buffer (), clock ()) in
  for _ = 0 to n  do 
    while Option.is_some (Atomic.get slot) do 
      Schedulr.Scheduler.yield () 
    done;
    Atomic.set slot (Some !data);
    data := (random_buffer (), clock ());  
  done;;

let global_latency_breach_count = Atomic.make 0;; 
let global_total_processing_time = Atomic.make 0;;
let global_total_processed = Atomic.make 0;;

let start_mixer ~n_packets ~n_legs () =
  let latency_breach_count = ref 0 in 
  let total_processing_time = ref 0 in 
  let total_processed = ref 0 in 
  let exchange_array = 
    Array.init n_legs (fun _ -> 
      let slot = Atomic.make None in 
      Schedulr.Scheduler.schedule (start_leg ~n:n_packets slot)
      |> ignore;
      slot) 
  in 
  let output = empty_buffer () in 
  for _ = 0 to n_packets do 
    let earliest_timestamp = ref Int.max_int in
    let start_timestamp = clock () in 
    for i = 0 to n_legs - 1 do 
      let element = 
        Array.get exchange_array i
      in 
      while Option.is_none (Atomic.get element) do 
        Schedulr.Scheduler.yield ()
      done;
      let (buffer, timestamp) = 
        match Atomic.exchange element None with 
        | None -> assert false 
        | Some v -> v 
      in 
      if (timestamp < !earliest_timestamp)
      then earliest_timestamp := timestamp;
      for j = 0 to buffer_size /2 - 3 do 
        let v_1 = Bytes.get_int32_be buffer (j*2) in 
        let v_2 = Bytes.get_int32_be output (j*2) in 
        Bytes.set_int32_be output (j*2) (Int32.add v_1 v_2)
      done; 
    done;
    let () = 
      let end_timestamp = clock () in 
      if !earliest_timestamp + 10_000_000 < end_timestamp 
      then latency_breach_count := !latency_breach_count + 1;
      let diff = end_timestamp - start_timestamp in 
      total_processing_time := !total_processing_time + diff;
      total_processed := !total_processed + 1;
    in  
    let _ = Sys.opaque_identity output in
    ();
  done;
  Atomic.fetch_and_add global_latency_breach_count !latency_breach_count
    |> ignore;
  Atomic.fetch_and_add global_total_processing_time !total_processing_time
    |> ignore;
  Atomic.fetch_and_add global_total_processed !total_processed 
    |> ignore;
  ();;

let log s = 
  Printf.printf "%s\n" s;
  Stdlib.(flush stdout);;

let workload ~n_mixers () = 
  Atomic.set global_latency_breach_count 0; 
  Atomic.set global_total_processing_time 0; 
  Atomic.set global_total_processed 0; 
  let promises = 
    Array.init n_mixers 
      (fun _ -> 
        Schedulr.Scheduler.schedule  
        (start_mixer 
          ~n_packets:10000
          ~n_legs:3)) 
  in 
  Array.iter Schedulr.Scheduler.await promises;;

let time f = 
  let time_start = Core.Time_ns.now () in 
  f ();
  let time_end = Core.Time_ns.now () in 
  let difference = Core.Time_ns.diff time_end time_start 
    |> Core.Time_ns.Span.to_int_ns 
  in 
  Printf.printf "time:%d\n" (difference/1000_000);
  Stdlib.flush_all ();;

let get_gc_stat () = 
  let gc = Gc.quick_stat ()
 in
  (gc.minor_words, gc.major_words)
  

let benchmark ~domains ~n_mixers (module Sched : Schedulr.Scheduler.S) = 
  Sched.init domains ~f:(fun () ->  
    Printf.printf "start\n";
    for i = 0 to 10 do 
      Printf.printf "iteration:%d\n" i;
      Unix.sleepf 0.1;
      let () = 
        let (start_minor_words, start_major_words) = get_gc_stat () in 
        time (workload ~n_mixers);
        let (end_minor_words, end_major_words) = get_gc_stat () in
        Printf.printf "minor_words:%.0f\nmajor_words:%.0f\n" 
          (end_minor_words -. start_minor_words)
          (end_major_words -. start_major_words)
      in
      while Sched.pending_tasks () != 0 do 
        Unix.sleepf 0.1;
      done;
      Printf.printf "breaches-rate:%d\n" 
        (Atomic.get global_latency_breach_count / n_mixers);
      Printf.printf "avg-processing-time:%d\n" 
        (Atomic.get global_total_processing_time 
          / Atomic.get global_total_processed);
      Stdlib.flush_all ();
    done;
    Printf.printf "done\n";
    Stdlib.flush_all ();
    Stdlib.exit 0);;


let () =
  let usage_msg = "benchmark -scheduler (FIFO|LIFO)" in 
  let anon_fun _ = failwith "no anon parameters expected" in
  let scheduler = ref "" in
  let num_of_domains = ref 0 in 
  let num_of_spawners = ref 0 in 
  let speclist =
    [("-scheduler", Arg.Set_string scheduler, "set scheduler algo");
    ("-num-of-domains", Arg.Set_int num_of_domains, "set num of domains");
    ("-num-of-spawners", Arg.Set_int num_of_spawners, "set num of spawners")] 
  in 
  Arg.parse speclist anon_fun usage_msg;
  let scheduler_module = Flags.parse_sched scheduler in 
  assert (0 < !num_of_domains && !num_of_domains < 512);
  assert (0 < !num_of_spawners && !num_of_spawners < 512);
  let domains = !num_of_domains in
  let n_mixers = !num_of_spawners in 
  benchmark ~domains ~n_mixers scheduler_module;;
