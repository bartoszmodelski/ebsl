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

let delay = 20_000_000 
let window = 10_000_000

let codec data = 
  Digestif.MD5.digest_bytes data 
  |> Sys.opaque_identity |> ignore;;

let start_leg ~n ~start_time slot () = 
  for i = 0 to n do 
    let data = random_buffer () in 
    codec data;
    while start_time + delay * i > clock () do 
      Schedulr.Scheduler.yield () 
    done;
    while Option.is_some (Atomic.get slot) do 
      Schedulr.Scheduler.yield () 
    done;
    Atomic.set slot (Some data);
  done;;

let send_packet_out ~deadline data () = 
  codec data;
  if clock () > deadline 
  then Reporting.Success.local_incr ()
;;

let n_legs_const = 2
let global_latency_breach_count = Atomic.make 0;; 

let start_mixer ~n_packets ~n_legs () =
  let start_time = clock () in 
  let exchange_array = 
    Array.init n_legs (fun _ -> 
      let slot = Atomic.make None in 
      Schedulr.Scheduler.schedule (start_leg ~n:n_packets ~start_time slot)
      |> ignore;
      slot) 
  in 
  for i = 0 to n_packets do 
    let output = empty_buffer () in 
    for i = 0 to n_legs - 1 do 
      let element = 
        Array.get exchange_array i
      in 
      while Option.is_none (Atomic.get element) do 
        Schedulr.Scheduler.yield ()
      done;
      let buffer = 
        match Atomic.exchange element None with 
        | None -> assert false 
        | Some v -> v 
      in 
      for j = 0 to buffer_size / 2 - 3 do 
        let v_1 = Bytes.get_int32_be buffer (j*2) in 
        let v_2 = Bytes.get_int32_be output (j*2) in 
        Bytes.set_int32_be output (j*2) (Int32.add v_1 v_2)
      done;
    done;
    let deadline = start_time + delay * i + window in 
    for _ = 0 to n_legs - 1 do 
      Schedulr.Scheduler.schedule (send_packet_out ~deadline output) |> ignore
    done; 
  done;;

let log s = 
  Printf.printf "%s\n" s;
  Stdlib.(flush stdout);;


let n_packets_const = 200

let workload ~n_mixers () = 
  let promises = 
    Array.init n_mixers 
      (fun _ -> 
        Schedulr.Scheduler.schedule  
        (start_mixer 
          ~n_packets:n_packets_const
          ~n_legs:n_legs_const)) 
  in 
  Array.iter Schedulr.Scheduler.await promises;;


let schedulers_count = ref 1

let benchmark ~domains ~n_mixers (module Sched : Schedulr.Scheduler.S) = 
  Reporting.Success.init 128;
  let schedulers = 
    Array.init !schedulers_count  (fun _ -> 
      Sched.init ~afterwards:`return (domains / !schedulers_count) ~f:(fun () -> ()))
  in 
  Printf.printf "{\"data\":[\n";
  let skip = 1 in
  for i = skip to 10 do 
    if i > skip then
    Printf.printf "{\"iteration\":%d," i;
    Unix.sleepf 0.1;
    Reporting.Success.unsafe_zero_out ();
    let finished = Atomic.make 0 in 
    for i = 0 to !schedulers_count - 1 do 
      let sched = Array.get schedulers i in 
      Sched.inject_task sched (fun () -> 
        workload ~n_mixers:(n_mixers / !schedulers_count) (); 
      Atomic.incr finished);
    done; 
    while (Atomic.get finished) < !schedulers_count do () done;
    let breach_count = Reporting.Success.unsafe_sum () in 
    Array.iter (fun sched -> 
    while Sched.pending_tasks sched != 0 do 
      Unix.sleepf 0.1;
    done) schedulers;
    if i > skip
    then Printf.printf "\"breaches-rate\":%f}" 
      (Int.to_float (breach_count / n_mixers / n_legs_const) /. 
      (Int.to_float n_packets_const));
    if skip < i && i < 10 
    then Printf.printf ",\n";
    Stdlib.flush_all ();
  done;
  Printf.printf "\n]}\n";
  Stdlib.flush_all ();;


let () =
  let usage_msg = "benchmark -scheduler (FIFO|LIFO)" in 
  let anon_fun _ = failwith "no anon parameters expected" in
  let scheduler = ref "" in
  let num_of_domains = ref 0 in 
  let num_of_spawners = ref 0 in 
  let speclist =
    [("-scheduler", Arg.Set_string scheduler, "set scheduler algo");
    ("-num-of-domains", Arg.Set_int num_of_domains, "set num of domains");
    ("-num-of-spawners", Arg.Set_int num_of_spawners, "set num of spawners");
    ("-num-of-scheds", Arg.Set_int schedulers_count, "set num of schedulers")] 
  in 
  Arg.parse speclist anon_fun usage_msg;
  let scheduler_module = Flags.parse_sched scheduler in 
  assert (0 < !num_of_domains && !num_of_domains < 512);
  assert (0 < !num_of_spawners);
  let domains = !num_of_domains in
  let n_mixers = !num_of_spawners in 
  benchmark ~domains ~n_mixers scheduler_module;;
